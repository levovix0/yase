import os, strutils, strformat
import cligen, terminal
import ast, eval3

var builtinNodes: seq[Node]

let nkSeq = nkNodeKind("seq", tNone)

builtin godPanel, nkNodeKind("god panel", tNone):
  try:
    if x.len < 1: return nkError(erIllformedAst, x)
    let rec = 2
    hideCursor()

    proc head(x: Node, indent: int): string =
      if x == nil: return "nil"
      if x.kind == nil: return "nil"

      result = if x.kind.childs.len > 0: x.kind.childs[0].asString else: "undocumented Node"

      if x.kind.childs.len > 1:
        let t = x.kind.childs[1]
        if t == tInt:
          result = &"{result} {x.asInt}"
        elif t == tString:
          result.add " "
          result.addQuoted x.asString
        elif t == tFloat:
          result = &"{result} {x.asFloat}"
        elif x.data.len != 0:
          result = &"{result} {x.data}"
      elif x.data.len != 0:
          result = &"{result} {x.data}"

      if x.childs.len > 0:
        result = result & ":"

      let id = cast[int](x).toHex
      let r = result
      let w = terminalWidth()
      result = " ".repeat(w)
      result[indent.min(w-1)..(indent + r.high).min(w-1)] = r[0..((indent + r.high).min(w-1) - indent.min(w-1))]
      if w > id.len + 2:
        result[^(id.len)..^1] = id
        result[^(id.len + 1)] = ' '


    proc drawUi(x: Node, buffer: Node, i: int) =
      eraseScreen()
      setCursorPos(0, 0)
      var ii = 0
      var s = @[x.head(0)]
      for i2, x in x.childs:
        if i2+1 == i:
          ii = s.len

        proc head2(x: Node, r: int): seq[string] =
          result = @[x.head(r * 2)]
          if r < rec:
            for x in x:
              result.add x.head2(r + 1)
        s.add x.head2(1)

      for i2, s in s:
        if i2 == ii:
          styledEcho(bgWhite, fgBlack, s, resetStyle)
        else:
          styledEcho(fgWhite, bgBlack, s, resetStyle)

      let h = terminalHeight()
      setCursorPos(0, h-3)
      styledEcho(fgWhite, bgBlack, " ".repeat(terminalWidth()), resetStyle)
      styledEcho(fgWhite, bgBlack, buffer.head(0), resetStyle)

    let root = x[0]
    var currentNode = root
    var path: seq[Node]
    var i: int
    var buffer: Node

    template selectedNode: Node =
      if i == 0: currentNode else: currentNode[i-1]

    while true:
      drawUi(currentNode, buffer, i)

      let k = getch()
      case k
      of 'q': break
      of 's':
        inc i
        if i > currentNode.len: i = 0
      of 'w':
        dec i
        if i < 0: i = currentNode.len
      of 'a':
        if path.len == 0: continue
        currentNode = path[^1]
        path.del path.high
        i = 0
      of 'd':
        if i == 0: continue
        path.add currentNode
        currentNode = currentNode[i-1]
        i = 0
      of 'D':
        if i == 0: continue
        currentNode.childs.delete i-1
        if i > currentNode.len: i = currentNode.len
      of 'S':
        writeFile "main.yase", root.serialize(builtinNodes)
      of 'b':
        buffer = selectedNode
      of 'k':
        selectedNode.kind = buffer
      of 'i':
        currentNode.childs.insert buffer, i
      of 'n':
        buffer = nkSeq()
      of 'e':
        buffer = eval selectedNode
      of '"':
        buffer = stdin.readLine
      of 'I':
        buffer =
          try: parseInt stdin.readLine
          except: 0
      of 'F':
        buffer =
          try: parseFloat stdin.readLine
          except: 0
      of '~':
        discard
      of 'h':
        eraseScreen()
        setCursorPos(0, 0)
        echo """
 q quit
 s move down
 w move up
 a move to parent
 d move to child
 D delete child
 S save
 b set buffer to current node
 k set kind to buffer node
 i insert child from buffer
 n create new node on buffer
 e eval current node to buffer
 " read string to buffer
 I read int to buffer
 F read float to buffer
 h help
 ~ do something"""
        discard getch()
      else: discard

  except:
    echo getCurrentExceptionMsg()
    echo getStackTrace()

builtinNodes = @[
  nkNodeKind,
  tNone,
  tString,
  nkString,
  tInt,
  tFloat,
  nkInt,
  nkFloat,
  nkError,

  cNone,
  cTrue,
  cFalse,
  eIf,
  eElse,
  erIllformedAst,
  eSeq,
  eIfStmt,
  eWhile,
  eConcat,

  godPanel,
  nkSeq,
]

proc yase(input: string = "main.yase") =
  var input = input
  if not input.fileExists and (input & ".yase").fileExists:
    input = input & ".yase"
  echo input.readFile.deserialize(builtinNodes).eval

when isMainModule:
  dispatch yase
