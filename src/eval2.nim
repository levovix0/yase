import tables, options, algorithm
import ast
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
    ## structure:
    ##   ident
    ##   value
  
  nkSet* = nkNodeKind("set", tNone)
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
    ##   (ident def | untyped ident def)... (args)


proc `[]`*(scope: Scope, s: Node): Option[Node] =
  var scope = scope
  while scope != nil:
    for k, v in scope.values:
      if k ==@ s: return some v
    scope = scope.parent

proc `[]=`*(scope: Scope, s: Node, v: Node): Option[Node] =
  var scope = scope
  while scope != nil:
    for k, mv in scope.values.mpairs:
      if k ==@ s:
        mv = v
        return
    scope = scope.parent

proc newScope*(parent: Scope = nil): Scope =
  Scope(parent: parent)


var builtins*: Table[Node, proc(x: Node): Node]

proc isValue(x: Node): bool =
  x.kind in [nkInt, nkString, nkBool, nkNode, nkProc]


var evalTable*: Table[Node, EvalProc]

template onKind*(kind: Node, body) =
  evalTable[kind] = proc(x {.inject.}: Node, scope {.inject.}: Scope = Scope()): Node =
    result = nkNone()
    body


proc eval*(x: Node, scope = Scope()): Node =
  try: evalTable[x.kind](x, scope)
  except: x


onKind nkIdent:
  return scope[x].get(x)

onKind nkCall:
  let f = x[0].eval

  let sf = scope[f]
  if sf.isSome and sf.get.kind == nkProc:
    let f = sf.get
    let v = x[1].eval
    ## todo
  
  if builtins.hasKey f:
    let v = x[1].eval
    if not x.isValue: return
    return builtins[f](v)

onKind nkVar:
  let (s, v) = (x[0], x[1].eval)
  for k, v in scope.values:
    if k ==@ s:
      scope.values[k] = v
  scope.values[s] = v

onKind nkSet:
  scope.values[x[0]] = x[1].eval

onKind nkScope:
  var scope = scope.newScope
  for x in x:
    result = x.eval(scope)
