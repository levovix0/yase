import ast, eval2, gui

discard eval nkWindow(
  "yase",
  gkSequence(
    gkFill(rgb(32, 32, 32)),

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
)
