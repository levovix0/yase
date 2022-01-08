import tables, options, algorithm
import ast

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


type Scope* = ref object
  parent*: Scope ## nilable
  values*: Table[Node, Node]

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


proc eval*(x: Node, scope = Scope()): Node =
  result = nkNone()
  
  case x.kind
  of nkIdent:
    return scope[x].get(x)

  of nkCall:
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
  
  of nkVar:
    let (s, v) = (x[0], x[1].eval)
    for k, v in scope.values:
      if k ==@ s:
        scope.values[k] = v
    scope.values[s] = v

  of nkSet:
    scope.values[x[0]] = x[1].eval
  
  of nkScope:
    var scope = scope.newScope
    for x in x:
      result = x.eval(scope)

  else: return
