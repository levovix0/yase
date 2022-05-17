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


converter toNode*(x: bool): Node =
  if x: cTrue else: cFalse


var builtins*: Table[Node, proc(x: Node): Node]

proc eval*(x: Node): Node =
  try: builtins[x.kind](x)
  except: x

template builtin*(name, node, body) {.dirty.} =
  let name* = node
  builtins[name] = proc(x: Node): Node = body


builtin eSeq, nkNodeKind("eval sequence, return last", tNone):
  result = cNone
  for x in x.childs:
    result = x.eval

builtin eIfStmt, nkNodeKind("if statement", tNone):
  result = cNone
  for x in x:
    if x.kind == eIf:
      if x.len != 2:
        return nkError(erIllformedAst, x)
      if x[0].eval == cTrue:
        return x[1].eval
    elif x.kind == eElse:
      if x.len != 1:
        return nkError(erIllformedAst, x)
      return x[0].eval

builtin eWhile, nkNodeKind("while", tNone):
  if x.len != 2:
    return nkError(erIllformedAst, x)
  result = cNone
  while x[0].eval == cTrue:
    result = x[1].eval

builtin eConcat, nkNodeKind("concat", tNone):
  if x.len < 1:
    return nkError(erIllformedAst, x)
  return x[0] & x.childs[1..^1]
