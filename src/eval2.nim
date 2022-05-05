import tables, options, sequtils
import ast, matching
export tables

type
  Scope* = ref object
    parent*: Scope ## nilable
    values*: Table[Node, Node]

  EvalProc* = proc(x: Node, scope = Scope()): Node

var
  nkNone* = nkNodeKind("none", tNone)
  nkIdent* = nkNodeKind("ident", tString)

  nkCall* = nkNodeKind("call", tNone)
    ## structure:
    ##   callable
    ##   args...

  nkScope* = nkNodeKind("scope", tNone)
    ## structure:
    ##   statements... (last returned)
  
  nkVar* = nkNodeKind("var", tNone)
    ## create new variable in current scope
    ## structure:
    ##   ident
    ##   value
  
  nkSet* = nkNodeKind("set", tNone)
    ## set value of existing variable
    ## structure:
    ##   ident
    ##   value
  
  nkIfStmt* = nkNodeKind("if stmt", tNone)
    ## structure:
    ##   if...
    ##   [else]
  
  nkIf* = nkNodeKind("if", tNone)
    ## structure:
    ##   condition
    ##   body
  
  nkElse* = nkNodeKInd("else", tNone)
    ## structure:
    ##   body
  
  nkWhile* = nkNodeKind("while", tNone)
    ## structure:
    ##   condition
    ##   body
  
  nkIdentDef* = nkNodeKind("ident def", tNone)
    ## structure:
    ##   ident
    ##   value match
    ##   [default]
  
  nkUntypedIdentDef* = nkNodeKind("untyped ident def", tNone)
    ## structure:
    ##   ident
    ##   [default]
  
  nkProc* = nkNodeKind("proc", tNone)
    ## structure:
    ##   body
    ##   scope
    ##   (ident def | untyped ident def)... (args)
  
  nkScopeLookup* = nkNodeKind("scope lookup", tNone)
    ## lookup in scope for a nearest matching value
    ## structure:
    ##   name match
    ##   value match


iterator rhierarchy*(x: Scope): Scope =
  var x = x
  while x != nil:
    yield x
    x = x.parent

proc `[]`*(scope: Scope, s: Node): Option[Node] =
  for scope in scope.rhierarchy:
    for k, v in scope.values:
      if k ==@ s: return some v

proc `[]=`*(scope: Scope, s: Node, v: Node) =
  for scope in scope.rhierarchy:
    for k, mv in scope.values.mpairs:
      if k ==@ s:
        mv = v
        return

proc newScope*(parent: Scope = nil): Scope =
  Scope(parent: parent)


template `!`*(x): Node = nkIdent{astToStr(x)}


var builtins*: Table[Node, proc(x: Node): Node]

proc isValue(x: Node): bool =
  x.kind in [nkInt, nkString, nkBool, nkNode, nkProc]


var evalTable*: Table[Node, EvalProc]

template onKind*(kind: Node, body) =
  evalTable[kind] = proc(x {.inject.}: Node, scope {.inject.}: Scope = Scope()): Node =
    result = nkNone()
    template eval(n: Node): Node {.used.} = n.eval(scope)
    body


proc eval*(x: Node, scope = Scope()): Node =
  try: evalTable[x.kind](x, scope)
  except: x


onKind nkIdent:
  return scope[x].get(x)


onKind nkScopeLookup:
  if x.len < 2: return nkError("illformed ast", x)
  let key = x[0]
  let val = x[1]
  for scope in scope.rhierarchy:
    for k, v in scope.values:
      if (k >- key) ==% true and (v >- val) ==% true:
        return v
  

onKind nkCall:
  let f = x[0].eval

  # proc call
  if f.kind == nkProc:
    let v = x.childs[1..^1].mapit(it.eval)
    for i, (v, s) in zip(v, f.childs[1..^1]):
      if v !>- s:
        return nkError("signature missmatch", i, v, s)
  
  # builtin call
  if builtins.hasKey f:
    let v = x[1].eval
    if not x.isValue: return
    return builtins[f](v)
  

onKind nkScope:
  var scope = scope.newScope
  for x in x:
    result = x.eval(scope)

onKind nkVar:
  let (s, nv) = (x[0], x[1].eval)
  for k, v in scope.values.mpairs:
    if k ==@ s:
      v = nv
      return
  scope.values[s] = nv

onKind nkSet:
  let (s, nv) = (x[0], x[1].eval)
  for scope in scope.rhierarchy:
    for k, v in scope.values.mpairs:
      if k ==@ s:
        v = nv
        return
  scope.values[s] = nv

onKind nkIfStmt:
  for a in x:
    if a.kind == nkElse: return a[0]
    elif a.kind == nkIf:
      if a[0].eval ==@ true: return a[1].eval
    else: return nkError("illformed ast", x)

onKind nkIf:
  if x[0].eval ==@ true: return x[1].eval

onKind nkWhile:
  while x[0].eval ==@ true:
    result = x[1].eval
