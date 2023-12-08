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

  moduleImport  <-  string * "#" * string
  nodeIdMap  <-  string * "=" * node
  exportNode  <-  "=" * string * "=" * node

  line  <-  (exportNode|moduleImport|nodeIdMap) * "\n"
  file  <-  "yase 1.0\n" * *(line)
]#
#[ example:
  yase 1.0
  #builtin
  1=(:tString,(2,:nkNodeKind,1:cNone=1;Hello World!))
  2=(:nkNodeKind,(:tString;my string))
  =root=1
  =text=1
  =my string=2
]#

import tables, algorithm
import ast

type
  SerializerState = object
    this*: Module
    modules*: seq[Module]
    builtins*: Table[Node, string]

    # pass 1
    toSerialize*: (
      when defined(yase_fastSave): CountTable[Node]
      else: OrderedTable[Node, int]
    )
    ids*: seq[Node]


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



proc excapeYase*(x: string): string =
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


proc save1*(m: Module, builtinNodes: openarray[(string, Node)]): string =
  result.add "yase 1.0\n"  # magic text
  
  var state = SerializerState(this: m)

  for (name, node) in builtinNodes:
    state.builtins[node] = name

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
        raise ValueError.newException("module not imported, but used")
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
        addSerializable(state, k)
        addSerializable(state, v)

  addSerializable(state, m.root)

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
          result.add ","
          result.add serialize(state, k)
          result.add "="
          result.add serialize(state, v)
        if n.data.len != 0:
          result.add ";"
          result.add n.data.toString.excapeYase
        result.add ")"
    
    elif n.module == nil:
      result.add state.modules.find(n.module).moduleId
      result.add ":"
      for x, k in state.builtins:
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


proc load1*(
  s: string,
  loadedModules: seq[Module],
  readModule: proc(path: string): string
): tuple[m: Module, newDeps: seq[Module], deps: seq[Module]] =
  ## todo
