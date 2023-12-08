# saving to to "human-readable" text files
# note: usualy node ids is +[0-9] numbers, module ids is +[A-Z] strings, builtin module is empty string
#[ format:
  string  <-  *(('\\' * 1) | !{'\n', '#', '=', ':', ';', ',', '(', ')'})
  
  nodeFromModule  <-  string * ":" * string
  
  nodeKind  <-  node
  nodeParam  <-  "," * node * "=" * node
  nodeChild  <-  "," * node
  nodeData  <-  ";" * string
  inlineNode  <-  "(" * nodeKind * *(nodeParam|nodeChild|nodeData) * ")"

  node <- (nodeFromModule | inlineNode | string)

  exportNode  <-  "=" * string * "=" * node
  moduleImport  <-  string * "#" * string
  nodeIdMap  <-  string * "=" * node
  exportParams  <-  nodeFromModule * *("," * node * "=" * node)

  line  <-  (exportNode|moduleImport|nodeIdMap|exportParams) * "\n"
  file  <-  "yase 1.0\n" * *(line)
]#
#[ example:
  yase 1.0
  #builtinexportParam
  1=(:tString,(2,:nkNodeKind,1:cNone=1;Hello World!))
  2=(:nkNodeKind,(:tString;my string))
  =root=1
  =text=1
  =my string=2
  :nodeKinds,2=2
]#

import tables, algorithm, strutils, os
import ast

type
  SerializerState = object
    this*: Module
    modules*: seq[Module]
    nilModule*: Table[Node, string]

    toSerialize*: (
      when defined(yase_fastSave): CountTable[Node]
      else: OrderedTable[Node, int]
    )
    ids*: seq[Node]
  
  NotFound* = object of CatchableError
  FormatError* = object of CatchableError


proc moduleId(i: int): string =
  when defined(yase_save_moduleIdAsNumbers):
    $(i + 1)
  
  elif defined(yase_save_moduleIdAsLowercaseLetters):
    if i == 0: return ""
    var i = i - 1
    while true:
      result.add char('a'.int + i mod 26)
      i = i div 26
      if i == 0: break
    reverse result

  else:
    if i == 0: return ""
    var i = i - 1
    while true:
      result.add char('A'.int + i mod 26)
      i = i div 26
      if i == 0: break
    reverse result



proc excapeYase(x: string): string =
  var i = 0
  while i < x.len:
    case x[i]
    of '\n': result.add "\\n"
    of '#': result.add "\\#"
    of '=': result.add "\\="
    of ':': result.add "\\:"
    of ';': result.add "\\;"
    of ',': result.add "\\,"
    of '(': result.add "\\("
    of ')': result.add "\\)"
    of '\\': result.add "\\\\"
    of '\0':
      var zeroCount = 1
      while zeroCount < 8 and i+1 < x.len and x[i + 1] == '\0':
        inc i
        inc zeroCount
      result.add:
        if zeroCount == 1: "\\0"
        else: '\\' & ('a'.int + zeroCount - 2).char
    elif x[i].int in 1..9:
      result.add "\\"
      result.add char('1'.int + x[i].int - 1)
    elif x[i].int in 10..31:
      result.add "\\"
      result.add char('A'.int + x[i].int - 10)
    else: result.add x[i]
    inc i


proc readStringYase(x: string, i: var int): string =
  while i < x.len and x[i] notin {'\n', '#', '=', ':', ';', ',', '(', ')'}:
    if x[i] == '\\':
      if i + 1 >= x.len: break
      inc i
      case x[i]
      of '\n': result.add '\n'
      of 'n': result.add '\n'
      of '#': result.add '#'
      of '=': result.add '='
      of ':': result.add ':'
      of ';': result.add ';'
      of ',': result.add ','
      of '(': result.add '('
      of ')': result.add ')'
      of '\\': result.add '\\'
      of ' ': # comment
        inc i
        var commentLevel = 1
        while i < x.len and (x[i] != '\n' or commentLevel > 1):
          if x[i] == '\\' and i + 1 < x.len and x[i + 1] == ' ':
            inc commentLevel
            inc i, 2
          elif x[i] == ' ' and i + 1 < x.len and x[i + 1] == '/':
            dec commentLevel
            inc i, 2
            if commentLevel == 0: break
          inc i
        dec i
      of {'0'..'9'}:  # number
        result.add char(x[i].int - '0'.int)
      of {'a'..'h'}:  # more than 1 \0
        for _ in -1..(x[i].int - 'a'.int):
          result.add '\0'
      of {'A'..char('A'.int + 21)}:  # numbers more than 9 (up to 31)
        result.add char(x[i].int - 'A'.int + 10)
      else: raise FormatError.newException("unknown escape sequence")
      inc i
    else:
      result.add x[i]
      inc i


proc readUpToNextLineAndSkipCommentsYase(x: string, i: var int) =
  while i < x.len and x[i] != '\n':
    if x[i] == '\\':
      if i + 1 >= x.len: break
      inc i
      if x[i] == ' ':
        inc i
        var commentLevel = 1
        while i < x.len and (x[i] != '\n' or commentLevel > 1):
          if x[i] == '\\' and i + 1 < x.len and x[i + 1] == ' ':
            inc commentLevel
            inc i, 2
          elif x[i] == ' ' and i + 1 < x.len and x[i + 1] == '/':
            dec commentLevel
            inc i, 2
            if commentLevel == 0: break
          inc i
        dec i
      else:
        inc i
    else:
      inc i
  inc i



proc save1*(m: Module, nilNodes: openarray[(string, Node)] = {:}): string =
  result.add "yase 1.0\n"  # magic text
  
  var state = SerializerState(this: m)

  for (name, node) in nilNodes:
    state.nilModule[node] = name

  # import modules
  for i, (x, instance) in m.imports:
    state.modules.add instance
    result.add i.moduleId
    result.add "#"
    result.add x.excapeYase
    result.add "\n"


  # --- pass 1 ---
  proc addSerializable(state: var SerializerState, n: Node) =
    if n.module != state.this:
      if n.module notin state.modules:
        raise ValueError.newException("module not imported, but used: " & (if n.module == nil: "nil" else: n.module.path))
      return
    
    if n in state.toSerialize:
      when defined(yase_fastSave):
        state.toSerialize.inc n
      else:
        state.toSerialize[n] += 1
    else:
      when defined(yase_fastSave):
        state.toSerialize.inc n
      else:
        state.toSerialize[n] = 1

      addSerializable(state, n.kind)

      for x in n.childs:
        addSerializable(state, x)
      
      for k, v in n.params:
        if (
          (k.module != state.this and k.module notin state.modules) or
          (v.module != state.this and v.module notin state.modules)
        ): continue  # extern param
        addSerializable(state, k)
        addSerializable(state, v)

  addSerializable(state, m.root)

  for n in m.extendNodes:
    for k, v in n.params:
      if not(k.module == state.this or v.module == state.this): continue  # intern param
      addSerializable(state, k)
      addSerializable(state, v)

  for n in m.exported.values:
    addSerializable(state, n)
  
  block setIds:
    for x, c in state.toSerialize:
      if c > 1:
        state.ids.add x
  

  # --- pass 2 ---
  proc serialize(state: var SerializerState, n: Node, force = false): string =
    if n == nil:
      # return "1:cNone"
      raise NilAccessDefect.newException("nil node in tree")

    if n.module == state.this:
      if (let i = state.ids.find(n); i != -1 and not force):
        return $(i + 1)
      else:
        result.add "("
        result.add serialize(state, n.kind)
        for x in n.childs:
          result.add ","
          result.add serialize(state, x)
        for k, v in n.params:
          if (
            (k.module != state.this and k.module notin state.modules) or
            (v.module != state.this and v.module notin state.modules)
          ): continue  # extern param
          result.add ","
          result.add serialize(state, k)
          result.add "="
          result.add serialize(state, v)
        if n.data.len != 0:
          result.add ";"
          result.add n.data.excapeYase
        result.add ")"
    
    elif n.module == nil:
      result.add state.modules.find(n.module).moduleId
      result.add ":"
      for x, k in state.nilModule:
        if x == n:
          result.add k.excapeYase
          return
      raise ValueError.newException("node is not builtin")

    else:
      result.add state.modules.find(n.module).moduleId
      result.add ":"
      for k, x in n.module.exported:
        if x == n:
          result.add k.excapeYase
          return
      raise ValueError.newException("node, accessed in this module, is not exported")
  
  for i, n in state.ids:
    result.addInt i+1
    result.add "="
    result.add serialize(state, n, force = true)
    result.add "\n"
  
  result.add "=root="
  result.add serialize(state, m.root)

  for k, n in m.exported:
    result.add "\n"
    result.add "="
    result.add k.excapeYase
    result.add "="
    result.add serialize(state, n)
  
  for n in m.extendNodes:
    result.add "\n"
    result.add state.modules.find(n.module).moduleId
    result.add ":"
    block findExportedNode:
      for k, x in n.module.exported:
        if x == n:
          result.add k.excapeYase
          break findExportedNode
      raise ValueError.newException("node, accessed in this module, is not exported")
    
    for k, v in n.params:
      result.add ","
      result.add serialize(state, k)
      result.add "="
      result.add serialize(state, v)



proc load1*(
  s: string,
  loadedModules: seq[Module],
  readModule: proc(path: string): string,
  path: string,
): tuple[m: Module, newDeps: seq[Module], deps: seq[Module]] =
  var i = 0
  result.m = Module()
  let thisModule = result.m
  
  # ---  eat magic text  ---
  while s[i] != '\n':
    inc i
  if s[0..4] != "yase ":
    raise FormatError.newException("failed to load: " & path & ", missing magic text")
  
  let version = s[5 .. i-1]
  if version != "1.0":
    raise FormatError.newException("failed to load: " & path & ", unsupported version: " & version)

  inc i
  
  let startI = i


  # ---  pass 1: get all deps and pre-create node for ids  ---
  var
    deps: OrderedTable[string, string]
    ids: Table[string, Node]

  proc getStringNear(s: string, i: int, n: int = 10): string =
    s[max(0, i-n) .. i-1].escape & " & " &
    ($s[i]).escape & " & " &
    s[i+1 .. min(i+n, s.len-1)].escape

  template raiseParseError =
    raise FormatError.newException("failed to load: " & path & ", parse error at byte: " & $i & ", near " & getStringNear(s, i))

  while i < s.len:
    if s[i] == '=':  # export node
      readUpToNextLineAndSkipCommentsYase(s, i)
      continue
    
    let ident = readStringYase(s, i)
    if i >= s.len: raiseParseError
    if s[i] == '#':
      inc i
      let path = readStringYase(s, i)
      if i >= s.len: raiseParseError
      if s[i] != '\n': raiseParseError
      inc i
      deps[ident] = path
    elif s[i] == '=':
      inc i
      ids[ident] = Node(module: result.m)
      readUpToNextLineAndSkipCommentsYase(s, i)
    else:
      readUpToNextLineAndSkipCommentsYase(s, i)
  
  i = startI


  # ---  pass 2: load deps  ---
  var
    modules: Table[string, Module]
  
  for ident, path in deps:
    block loadModule:
      for m in loadedModules:
        if m.path == path:
          modules[ident] = m
          result.deps.add m
          result.m.imports.add (file: path, instance: m)
          break loadModule
      let m = load1(readModule(path), loadedModules & result.newDeps, readModule, path)
      modules[ident] = m.m
      result.newDeps.add m.newDeps
      result.newDeps.add m.m
      result.m.imports.add (file: path, instance: m.m)


  # ---  pass 3: load everything  ---

  proc getNode(s: string, i: var int): Node =
    let ident = readStringYase(s, i)
    if i < s.len and s[i] == ':':
      inc i
      let nId = readStringYase(s, i)
      modules[ident].exported[nId]
    else:
      ids[ident]


  proc loadNode(n: Node, s: string, i: var int) =
    proc theNode(s: string, i: var int): Node =
      let ident = readStringYase(s, i)
      if i < s.len and s[i] == '(':
        inc i
        var newNode = Node(module: thisModule)
        loadNode(newNode, s, i)
        newNode
      elif i < s.len and s[i] == ':':
        inc i
        let nId = readStringYase(s, i)
        try: modules[ident].exported[nId]
        except: raise NotFound.newException("no such exported node: " & modules[ident].path & ":" & nId)
      else:
        ids[ident]
    
    n.kind = theNode(s, i)
    
    while true:
      if i >= s.len: raiseParseError
      case s[i]
      of ';':  # data
        inc i
        n.data = readStringYase(s, i)

      of ',':  # child or param
        inc i
        let a = theNode(s, i)
        if i < s.len and s[i] == '=':  # param
          inc i
          let b = theNode(s, i)
          n.params[a] = b
        else:  # child
          n.childs.add a
      
      of ')':
        inc i
        return

      else: raiseParseError

  while i < s.len:
    if s[i] == '=':  # export node
      inc i
      let exportName = readStringYase(s, i)
      if i >= s.len: raiseParseError
      if s[i] != '=': raiseParseError
      inc i
      if i >= s.len: raiseParseError
      if s[i] == '(':
        inc i
        var newNode = Node(module: thisModule)
        loadNode(newNode, s, i)
        if exportName == "root":
          result.m.root = newNode
        else:
          result.m.exported[exportName] = newNode
      else:
        if exportName == "root":
          result.m.root = ids[readStringYase(s, i)]
        else:
          result.m.exported[exportName] = ids[readStringYase(s, i)]
      
      continue
    
    let ident = readStringYase(s, i)
    if i >= s.len: raiseParseError
    case s[i]
    of '#':
      readUpToNextLineAndSkipCommentsYase(s, i)
    
    of '=':  # load node
      inc i
      if i >= s.len: raiseParseError
      if s[i] != '(': raiseParseError
      inc i
      loadNode(ids[ident], s, i)
      readUpToNextLineAndSkipCommentsYase(s, i)
    
    of ':':  # load params
      inc i
      if i >= s.len: raiseParseError
      let nId = readStringYase(s, i)
      let n1 = modules[ident].exported[nId]

      while i < s.len:
        if i >= s.len or s[i] == '\n': break
        if s[i] != ',': raiseParseError
        inc i
        var k: Node
        if i < s.len and s[i] == '(':
          k = Node(module: thisModule)
          loadNode(k, s, i)
        else:
          k = getNode(s, i)

        if i >= s.len: raiseParseError
        if s[i] != '=': raiseParseError
        inc i
        var v: Node
        if i < s.len and s[i] == '(':
          v = Node(module: thisModule)
          loadNode(v, s, i)
        else:
          v = getNode(s, i)
        
        n1.params[k] = v
    
    else: raiseParseError



proc default_readModule*(
  paths: openarray[(string, seq[string])] = {
    "": @["."],
    "std": @["lib"],
    # "pkg": @["~/.local/lib/yase/pkg/module1", ...],
  }
): proc(path: string): string =
  let paths = paths.toOrderedTable

  result = proc(path: string): string =
    for k, a in paths:
      if k != "" and path.startsWith(k & "/"):
        let path = path[k.len+1 .. ^1]
        for x in a:
          let p = x / path
          if p.fileExists:
            return p.readFile
    for x in paths.getOrDefault("", @["."]):
      let p = x / path
      if p.fileExists:
        return p.readFile
    raise NotFound.newException("module not found: " & path)
