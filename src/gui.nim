import siwin, pixie, vmath
import ast, eval2
export pixie

type
  DrawScope* = ref object
    parent*: DrawScope ## nilable
    scope*: Scope
    clipRect*: Rect

let
  tRgba* = Node()
    ## first 4 bytes are r, g, b, a in 0..255 range

  nkColor* = nkNodeKind("color", tRgba)
    ## rgba color

  gkFillColor* = nkNodeKind("fill color =", tNone)
    ## set fill color of context
    ## structure:
    ##   fill color (gkColor)
  
  gkFill* = nkNodeKind("fill", tNone)
    ## fill whole image
    ## structure:
    ##   fill color (gkColor)

  gkRect* = nkNodeKind("rect", tNone)
    ## draw rect
    ## structure:
    ##   x
    ##   y
    ##   w
    ##   h
  
  gkMove* = nkNodeKind("move", tNone)
    ## translate context
    ## structure:
    ##   x
    ##   y
  
  gkScale* = nkNodeKind("scale", tNone)
    ## scale context
    ## structure:
    ##   x
    ##   y
  
  gkRotate* = nkNodeKind("rotate", tNone)
    ## rotate context
    ## structure:
    ##   angle
  
  gkSequence* = nkNodeKind("sequence", tNone)
  
  gkFrame* = nkNodeKind("frame", tNone)
    ## same as sequence, but pushes context before drawing, and pops it after
  
  gkText* = nkNodeKind("text", tNone)
    ## structure:
    ##   x
    ##   y
    ##   text (string)
    ##   h (font size)
    ##   bg (nkColor) | nkNone

  gkClip* = nkNodeKind("clip", tNone)
    ## clip content (rectangle shape)
    ## structure:
    ##   x
    ##   y
    ##   w
    ##   h
    ##   content

  nkWindow* = nkNodeKind("window", tNone)
    ## creates window and run event loop
    ## structure:
    ##   title
    ##   draw
    ##   onEvent (e:ek* in scope)
  
  ekKeyup* = nkNodeKind("keyup", tNone)
    ## structure:
    ##   keycode
  
  ekKeydown* = nkNodeKind("keydown", tNone)
    ## structure:
    ##   keycode
  
  ekTick* = nkNodeKind("tick", tNone)
    ## structure:


converter toNode*(c: ColorRgba): Node =
  nkColor{c}

converter toNode*(c: ColorRgbx): Node =
  nkColor{c.rgba}

converter toNode*(c: ColorRgb): Node =
  nkColor{rgba(c.r, c.g, c.b, 255)}

converter toNode*(c: Color): Node =
  nkColor{c.rgba}

proc asRgba*(x: Node): ColorRgba =
  if x.data.len == ColorRgba.sizeof:
    cast[ptr ColorRgba](x.data[0].addr)[]
  else: rgba(0, 0, 0, 0)

proc toFloat(x: Node): float =
  if x.kind[1] == tInt: x.asInt.float
  else: x.asFloat


proc draw*(r: Context, x: Node, ds: DrawScope) =
  proc draw(x: Node) = r.draw x, ds
  proc eval(x: Node): Node = x.eval(ds.scope)

  case x.kind
  of gkSequence:
    for x in x: draw x
  
  of gkFillColor:
    r.fillStyle = x[0].eval.asRgba
  
  of gkFill:
    r.image.fill x[0].eval.asRgba
  
  of gkRect:
    r.fillRect x[0].eval.toFloat, x[1].eval.toFloat, x[2].eval.toFloat, x[3].eval.toFloat
  
  of gkFrame:
    r.save
    for x in x: draw x
    r.restore
  
  of gkMove:
    r.translate x[0].eval.toFloat, x[1].eval.toFloat
  
  of gkScale:
    r.scale x[0].eval.toFloat, x[1].eval.toFloat
  
  of gkRotate:
    r.rotate x[0].eval.toFloat
  
  of gkText:
    ## todo
  
  of gkClip:
    ## todo
  
  else: discard x.eval


onKind nkWindow:
  var window = newWindow(title = x[0].eval.asString, w = 600, h = 400)
  var image = newImage(600, 400)
  discard eval nkVar(!w, 600.0)
  discard eval nkVar(!h, 400.0)

  proc push(e: Node) =
    let scope = scope.newScope
    scope.values[!e] = e
    discard x[2].eval(scope)

  window.onRender = proc(e: RenderEvent) =
    image.newContext.draw x[1].eval, DrawScope(
      scope: scope,
      clipRect: rect(vec2(), ivec2(window.size.x.int32, window.size.y.int32).vec2),
    )
    window.drawImage image.data

  window.onResize = proc(e: ResizeEvent) =
    if e.size.x * e.size.y == 0: return
    image = newImage(e.size.x, e.size.y)
    discard eval nkSet(!w, e.size.x.float)
    discard eval nkSet(!h, e.size.x.float)
    redraw window

  window.onKeyDown = proc(e: KeyEvent) =
    push ekKeydown(e.key.ord)
    redraw window

  window.onKeyUp = proc(e: KeyEvent) =
    push ekKeyup(e.key.ord)
    redraw window

  window.onTick = proc(e: TickEvent) =
    push ekTick()
    redraw window

  run window
