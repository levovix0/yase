import tables
import ast

type
  Evalable* = object
    case isBuiltin*: bool
    of true:
      f: proc(x: Node): Node
    of false:
      n: Node

  EvalTable* = Table[Node, Evalable]

var evalTable*: EvalTable

proc eval*(x: Node): Node =
  try:
    let e = evalTable[x.kind]
    if e.isBuiltin: e.f(x)
    else: (e.n & x).eval
  except: x


template builtin(name, node, body) {.dirty.} =
  let name* = node
  evalTable[name] = Evalable(isBuiltin: true, f: proc(x: Node): Node = body)


let
  cNone* = Node "none"
  cTrue* = Node "true"
  cFalse* = Node "false"

  eIf* = nkNodeKind("if", tNone)
  eElse* = nkNodeKind("else", tNone)

  erIllformedAst = "illformed ast"


converter toNode*(x: bool): Node =
  if x: cTrue else: cFalse


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
