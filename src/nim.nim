import macros, unicode, sequtils
import fusion/matching, fusion/astdsl, regex
import ast

macro declareNodeKinds =
  result = newStmtList()
  for x in "NimNodeKind".bindSym.getImpl[2][1..^1]:
    result.add: buildAst(letSection):
      identDefs:
        postfix ident"*":
          ident "n" & x.strVal
        bindSym"Node"
        call bindSym"nkNodeKind":
          newLit x.strVal[3..^1].replace(re"([a-z])([A-Z])", "$1 $2").toLower
          bindSym"tNone"

declareNodeKinds()

nnnkIdent[1] = tString
nnnkStrLit[1] = tString
nnnkIntLit[1] = tInt
nnnkFloatLit[1] = tFloat


macro node(args: varargs[untyped]): Node =
  proc cv(x: NimNode, replace: openarray[(NimNode, NimNode)]): NimNode =
    let xi = replace.mapit(it[0]).find(x)
    if xi != -1:
      replace[xi][1]
    else:
      buildAst(call):
        ident "n" & $x.kind
        for x in x: cv(x, replace)
        case x
        of IntLit(intVal: @v):
          exprEqExpr ident"data":
            call bindSym"toBytes":
              newLit v
        of StrLit(strVal: @v), Ident(strVal: @v):
          exprEqExpr ident"data":
            call bindSym"toBytes":
              newLit v
        of FloatLit(floatVal: @v):
          exprEqExpr ident"data":
            call bindSym"toBytes":
              newLit v

  var repl: seq[(NimNode, NimNode)]

  for a in args[0..^2]:
    case a.kind
    of nnkExprEqExpr:
      repl.add (a[0], a[1])
    of nnkIdent:
      repl.add (a, a)
    else: error("invalid argument kind: " & $a.kind, a)
  
  cv(args[^1], repl)

echo node do:
  proc f(x: int): float = 10.7
