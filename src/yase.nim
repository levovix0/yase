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

  stdKind* = nkNodeKind(".kind", tNone)


onKind stdEcho:
  let s = x[0].eval
  if s.kind == nkString:
    echo s.asString
  else:
    echo s

onKind stdEqual:
  return x[0].eval ==@ x[1].eval

onKind stdKind:
  return x[0].eval.kind()

onKind stdBinaryPlus:
  return x[0].eval.asFloat + x[1].eval.asFloat
template `+`*(a, b: Node): Node = stdBinaryPlus(a, b)

onKind stdBinaryMinus:
  return x[0].eval.asFloat - x[1].eval.asFloat
template `-`*(a, b: Node): Node = stdBinaryMinus(a, b)

onKind stdBinaryProd:
  return x[0].eval.asFloat * x[1].eval.asFloat
template `*`*(a, b: Node): Node = stdBinaryProd(a, b)

onKind stdBinaryDivide:
  return x[0].eval.asFloat / x[1].eval.asFloat
template `/`*(a, b: Node): Node = stdBinaryDivide(a, b)


onKind stdUnaryMinus:
  return -x[0].eval.asFloat
template `-`*(a: Node): Node = stdUnaryMinus(a)


onKind stdNot:
  return not x[0].eval.asBool
template `not`*(a: Node): Node = stdNot(a)


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

  nkWindow(
    "yase",
    gkFrame(
      gkFill(rgb(32, 32, 32)),
      gkScale(!scale, !scale),
      gkMove(- !x, - !y),

      gkFrame(
        gkFillColor(rgb(100, 100, 255)),
        gkMove(200, 100),
        gkScale(0.5, 0.5),
        gkRotate(45),
        gkRect(0, 0, 100, 100),
      ),

      gkFillColor(rgb(100, 255, 100)),
      gkRect(0, 0, 100, 100),
    ),
    nkScope(block:
      # stdEcho(!e),
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
            nkSet(!scale, !scale / 1.05),
            nkSet(!x, !x - !w / !scale * 0.025),
            nkSet(!y, !y - !h / !scale * 0.025),
          ))),
          nkIfStmt(nkIf( !e_pressed, nkScope(
            nkSet(!scale, !scale * 1.05),
            nkSet(!x, !x + !w / !scale * 0.025),
            nkSet(!y, !y + !h / !scale * 0.025),
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
      )
    ),
  )
)
