import siwin, pixie
import ast, eval2
export pixie

let
  tRgba* = Node()
    ## first 4 bytes are r, g, b, a in 0..255 range

  gkColor* = nkNodeKind("color", tRgba)
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
  
  gkBlock* = nkNodeKind("block", tNone)
    ## same as sequence, but pushes context before drawing, and pops it after
  
  nkWindow* = nkNodeKind("window", tNone)
    ## creates window and run event loop
    ## structure:
    ##   title
    ##   draw


converter toNode*(c: ColorRgba): Node =
  gkColor{c}

converter toNode*(c: ColorRgbx): Node =
  gkColor{c.rgba}

converter toNode*(c: ColorRgb): Node =
  gkColor{rgba(c.r, c.g, c.b, 255)}

converter toNode*(c: Color): Node =
  gkColor{c.rgba}

proc asRgba*(x: Node): ColorRgba =
  if x.data.len == ColorRgba.sizeof:
    cast[ptr ColorRgba](x.data[0].addr)[]
  else: rgba(0, 0, 0, 0)

proc toFloat(x: Node): float =
  if x.kind[1] == tInt: x.asInt.float
  else: x.asFloat


proc draw*(r: Context, x: Node) =
  case x.kind
  of gkSequence:
    for x in x: r.draw x
  
  of gkFillColor:
    r.fillStyle = x[0].eval.asRgba
  
  of gkFill:
    r.image.fill x[0].eval.asRgba
  
  of gkRect:
    r.fillRect x[0].eval.toFloat, x[1].eval.toFloat, x[2].eval.toFloat, x[3].eval.toFloat
  
  of gkBlock:
    r.save
    for x in x: r.draw x
    r.restore
  
  of gkMove:
    r.translate x[0].eval.toFloat, x[1].eval.toFloat
  
  of gkScale:
    r.scale x[0].eval.toFloat, x[1].eval.toFloat
  
  of gkRotate:
    r.rotate x[0].eval.toFloat
  
  else: discard


onKind nkWindow:
  var image: Image
  
  run newWindow(title=x[0].asString):
    resize as (w, h):
      image = newImage(w, h)

    render:
      image.newContext.draw x[1].eval
      window.drawImage image.data

    keyup esc:
      close window
