import os, strutils, strformat, sequtils, terminal
import argparse
import ast, eval

var builtinNodes: seq[Node]

let nkSeq = nkNodeKind("seq", tNone)

builtin godPanel, nkNodeKind("god panel", tNone):
  try:
    if x.len < 2: return nkError(erIllformedAst, x)
    let rec = 2

    proc head(x: Node, indent: int): string =
      if x == nil:
        return " ".repeat(indent) & "nil"

      result =
        if x.kind != nil and x.kind.childs.len > 0:
          x.kind.childs[0].asString
        elif x.kind == nil: "nil"
        elif x.kind.kind == nkString: x.kind.asString
        else: "undocumented Node"

      if x.kind != nil and x.kind.childs.len > 1:
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

      let h = terminalHeight()
      if s.len >= h-3:
        let start = (ii-(h div 5)).max(0).min(s.high)
        s = s[start .. (start + h-3).min(s.high)]
        ii -= start

      for i2, s in s:
        if i2 == ii:
          styledEcho(bgWhite, fgBlack, s, resetStyle)
        else:
          styledEcho(fgWhite, bgBlack, s, resetStyle)

      setCursorPos(0, h-3)
      styledEcho(fgWhite, bgBlack, " ".repeat(terminalWidth()), resetStyle)
      styledEcho(fgWhite, bgBlack, buffer.head(0), resetStyle)

    let root = x[0]
    var currentNode = root
    var path: seq[(Node, int)]
    var i: int
    var buffer: Node

    template selectedNode: Node =
      if i == 0: currentNode else: currentNode[i-1]

    while true:
      drawUi(currentNode, buffer, i)

      let k = getch()
      case k
      of 'q':
        eraseScreen()
        setCursorPos(0, 0)
        break
      of 's':
        inc i
        if i > currentNode.len: i = 0
      of 'w':
        dec i
        if i < 0: i = currentNode.len
      of 'a':
        if path.len == 0: continue
        (currentNode, i) = path[^1]
        path.del path.high
        i = i.max(0).min(currentNode.childs.len)
      of 'd':
        if i == 0: continue
        path.add (currentNode, i)
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
        if buffer == nil: continue
        currentNode.childs.insert buffer, i
        inc i
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
      of '[':
        if i notin 2..currentNode.childs.len: continue
        swap currentNode[i-1], currentNode[i-2]
        dec i
      of ']':
        if i notin 1..currentNode.childs.high: continue
        swap currentNode[i-1], currentNode[i]
        inc i
      of '~':
        buffer = nkSeq(builtinNodes.filterit(it.kind == nkNodeKind))
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
      else:
        for x in x[1]:
          if x.kind == nkString and x.len >= 1 and x.asString == $k:
            discard x[0].eval

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
  nil,
]

var yase = newParser:
  arg("input", default=some"main.yase", help="eval file")
  run:
    var input = opts.input
    if not input.fileExists and (input & ".yase").fileExists:
      input = input & ".yase"
    echo input.readFile.deserialize(builtinNodes).eval

when isMainModule:
  try:
    run yase
  except UsageError as e:
    stderr.writeLine getCurrentExceptionMsg()
    quit(1)
