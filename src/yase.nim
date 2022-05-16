import sequtils
import siwin
import ast, eval2, gui

let
  stdEcho* = nkNodeKind("echo", tNone)
  stdEqual* = nkNodeKind("==", tNone)
  
  stdBinaryPlus* = nkNodeKind("+", tNone)
  stdBinaryMinus* = nkNodeKind("-", tNone)
  stdBinaryProd* = nkNodeKind("*", tNone)
  stdBinaryDivide* = nkNodeKind("/", tNone)
  
  stdUnaryMinus* = nkNodeKind("-", tNone)

  stdNot* = nkNodeKind("not", tNone)

  stdKind* = nkNodeKind("kind(Node)", tNone)
  stdLen* = nkNodeKind("len(Node)", tNone)
  stdPass* = nkNodeKind("pass(Node)", tNone)
  stdEval* = nkNodeKind("eval(Node)", tNone)
  `std[]`* = nkNodeKind("Node[int]", tNone)
  stdNewNode* = nkNodeKind("newNode(varargs[Node])", tNone)

  stdRgb* = nkNodeKind("rgb(int, int, int)", tNone)

  stdOnKind* = nkNodeKind("onKind(kind: Node, body: Node)", tNone)


onKind stdEcho:
  let s = x[0].eval
  if s.kind == nkString:
    echo s.asString
  else:
    echo s

onKind stdEqual:
  return x[0].eval ==@ x[1].eval

template mkNumericBinaryOp(k, f) {.dirty.} =
  onKind k:
    let
      a = x[0].eval
      b = x[1].eval
    if a.kind != b.kind: return nkError("missmatch kinds", a, b)
    return if a.kind == nkFloat:
      Node f(a.asFloat, b.asFloat)
    elif a.kind == nkInt:
      Node f(a.asInt, b.asInt)
    else:
      nkError("invalid kind", a)
  template f*(a, b: Node): Node = k(a, b)

mkNumericBinaryOp stdBinaryPlus, `+`
mkNumericBinaryOp stdBinaryMinus, `-`
mkNumericBinaryOp stdBinaryProd, `*`
mkNumericBinaryOp stdBinaryDivide, `/`


onKind stdUnaryMinus:
  return -x[0].eval.asFloat
template `-`*(a: Node): Node = stdUnaryMinus(a)


onKind stdNot:
  return not x[0].eval.asBool
template `not`*(a: Node): Node = stdNot(a)


onKind stdKind:
  return x[0].eval.kind()

onKind stdLen:
  return x[0].eval.len

onKind stdPass:
  return x[0]

onKind stdEval:
  return x[0].eval.eval

onKind `std[]`:
  return x[0].eval[x[1].eval.asInt]

onKind stdNewNode:
  return (x[0].eval)(x.childs[1..^1].mapit(it.eval))


onKind stdRgb:
  return rgb(
    x[0].eval.asInt.min(255).max(0).byte,
    x[1].eval.asInt.min(255).max(0).byte,
    x[2].eval.asInt.min(255).max(0).byte
  )


onKind stdOnKind:
  let y = x
  onKind y[0].eval:
    return nkScope(
      @[nkVar(!x, x)] &
      y.childs[1..^1]
    ).eval


discard eval nkScope(
  nkVar(!x, 0.0),
  nkVar(!y, 0.0),
  nkVar(!scale, 1.0),
  
  nkVar(!w_pressed, false),
  nkVar(!a_pressed, false),
  nkVar(!s_pressed, false),
  nkVar(!d_pressed, false),
  nkVar(!q_pressed, false),
  nkVar(!e_pressed, false),
  nkVar(!currentNode, stdPass(nkError("a", "b", "c", "d"))),
  nkVar(!col, nkNone()),

  nkVar(!onCurrentNodeChanged, stdPass(
    nkSet(!col, stdLen(!currentNode) * 32),
  )),

  stdEval(!onCurrentNodeChanged),

  nkVar(!nkProperty, stdPass(nkNodeKind("property", tNone))),
  nkVar(!nkTuple, stdPass(nkNodeKind("property", tNone))),
  stdOnKind(!nkProperty,
    nkVar(`std[]`(!x, 0), ),
  ),

  nkWindow(
    "yase",
    gkFrame(
      gkFill(rgb(32, 32, 32)),
      gkScale(!scale, !scale),
      gkMove(- !x, - !y),

      gkFrame(
        gkFillColor(stdRgb(!col, !col, !col)),
        gkMove(200, 100),
        gkScale(0.5, 0.5),
        gkRotate(45),
        gkRect(0, 0, 100, 100),
      ),

      gkFillColor(rgb(100, 255, 100)),
      gkRect(0, 0, 100, 100),
    ),
    nkScope(
      nkIfStmt(nkIf( stdNot(stdEqual(stdKind(!e), ekTick())),
        #stdEcho(!e),
      )),
      block:
        template then(x, body): Node =
          nkIf( stdEqual(!e, x),
            body
          )

        nkIfStmt(
          ekTick().then nkScope(
            nkIfStmt(nkIf( !w_pressed, nkScope(
              nkSet(!y, !y - 10.0 / !scale),
            ))),
            nkIfStmt(nkIf( !a_pressed, nkScope(
              nkSet(!x, !x - 10.0 / !scale),
            ))),
            nkIfStmt(nkIf( !s_pressed, nkScope(
              nkSet(!y, !y + 10.0 / !scale),
            ))),
            nkIfStmt(nkIf( !d_pressed, nkScope(
              nkSet(!x, !x + 10.0 / !scale),
            ))),
            nkIfStmt(nkIf( !q_pressed, nkScope(
              nkSet(!x, !x + (!w / !scale - !w / !scale * 1.05) / 2.0),
              nkSet(!y, !y + (!h / !scale - !h / !scale * 1.05) / 2.0),
              nkSet(!scale, !scale / 1.05),
            ))),
            nkIfStmt(nkIf( !e_pressed, nkScope(
              nkSet(!x, !x + (!w / !scale - !w / !scale / 1.05) / 2.0),
              nkSet(!y, !y + (!h / !scale - !h / !scale / 1.05) / 2.0),
              nkSet(!scale, !scale * 1.05),
            ))),
          ),

          ekKeyup(Key.w.ord).then nkSet(!w_pressed, false),
          ekKeydown(Key.w.ord).then nkSet(!w_pressed, true),

          ekKeyup(Key.a.ord).then nkSet(!a_pressed, false),
          ekKeydown(Key.a.ord).then nkSet(!a_pressed, true),

          ekKeyup(Key.s.ord).then nkSet(!s_pressed, false),
          ekKeydown(Key.s.ord).then nkSet(!s_pressed, true),

          ekKeyup(Key.d.ord).then nkSet(!d_pressed, false),
          ekKeydown(Key.d.ord).then nkSet(!d_pressed, true),

          ekKeyup(Key.q.ord).then nkSet(!q_pressed, false),
          ekKeydown(Key.q.ord).then nkSet(!q_pressed, true),

          ekKeyup(Key.e.ord).then nkSet(!e_pressed, false),
          ekKeydown(Key.e.ord).then nkSet(!e_pressed, true),
        ),
    ),
  )
)
