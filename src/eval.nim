import tables
import ast
export tables

let
  cNone* = Node "none"
  cTrue* = Node "true"
  cFalse* = Node "false"

  eIf* = nkNodeKind("if", tNone)
  eElse* = nkNodeKind("else", tNone)

  erIllformedAst* = Node "illformed ast"
  erType* = Node "type error"

  egStack* = Node "global eval stack"


converter toNode*(x: bool): Node =
  if x: cTrue else: cFalse


var builtins*: Table[Node, proc(x: Node): Node]
var lets*: Table[int, Table[Node, Node]]

proc eval*(x: Node): Node =
  if x.kind in builtins:
    return builtins[x.kind](x)
  else:
    if x.kind.kind != nkNodeKind or x.kind.len < 3 or x.kind[2] == cNone: return x
    egStack.childs.add x  # push
    result = x.kind[2].eval
    lets.del egStack.len  # cleanup lets
    egStack.childs.setLen max(egStack.childs.high, 0)  # pop

template builtin*(name, node, body) {.dirty.} =
  let name* = node
  builtins[name] = proc(x: Node): Node = body


builtin eSeq, nkNodeKind("eval sequence, return last", tNone):
  result = cNone
  for x in x.childs:
    result = x.eval

builtin eIfStmt, nkNodeKind("if statement", tNone):
  result = cNone
  for y in x:
    if y.kind == eIf:
      if y.len != 2:
        return nkError(erIllformedAst, x, y)
      let ny = y[0].eval
      if ny == cTrue:
        return y[1].eval
      elif ny != cFalse: return nkError(erType, x, y, ny)
    elif y.kind == eElse:
      if y.len != 1:
        return nkError(erIllformedAst, x, y)
      return y[0].eval

builtin eWhile, nkNodeKind("while", tNone):
  if x.len != 2:
    return nkError(erIllformedAst, x)
  result = cNone
  while x[0].eval == cTrue:
    result = x[1].eval

builtin eLet, nkNodeKind("let", tNone):
  if x.len != 1: return nkError(erIllformedAst, x)
  
  if egStack.len notin lets:
    lets[egStack.len] = {x[0]: x[0].eval}.toTable
  
  if x[0] notin lets[egStack.len]:
    lets[egStack.len][x[0]] = x[0].eval
  
  lets[egStack.len][x[0]]
