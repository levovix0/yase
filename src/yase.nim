import os, strutils, strformat, sequtils, terminal, unicode
import argparse
import ast, eval

var builtinNodes: seq[Node]

let
  erIndex* = Node "index error"
  
  nkSeq* = nkNodeKind("seq", tNone)

builtin stdInsert, nkNodeKind("insert node", tNone):
  if x.len < 3: return nkError(erIllformedAst, x)
  result = cNone
  let ni = x[1].eval
  if ni.kind != nkInt: return nkError(erType, x, ni)
  let i = ni.asInt
  x[0].eval.childs.insert x[2].eval, i

builtin stdDelete, nkNodeKind("delete node", tNone):
  if x.len < 2: return nkError(erIllformedAst, x)
  result = cNone
  let ni = x[1].eval
  if ni.kind != nkInt: return nkError(erType, x, ni)
  let i = ni.asInt
  let nx = x[0].eval
  if i notin 0..<nx.len: return nkError(erIndex, x)
  nx.childs.delete i

builtin stdPass, nkNodeKind("pass", tNone):
  if x.len < 1: return nkError(erIllformedAst, x)
  x[0]

builtin stdEval, nkNodeKind("eval", tNone):
  if x.len < 1: return nkError(erIllformedAst, x)
  x[0].eval.eval

builtin stdGet, nkNodeKind("get child", tNone):
  if x.len < 1: return nkError(erIllformedAst, x)
  let nx = x[0].eval
  if x.len == 1: return nx[0]
  
  let ni = x[1].eval
  if ni.kind != nkInt: return nkError(erType, x, ni)
  let i = ni.asInt
  if i notin 0..<nx.len: return nkError(erIndex, x, nx, ni)
  nx[i]

builtin stdLen, nkNodeKind("len", tNone):
  if x.len < 1: return nkError(erIllformedAst, x)
  x[0].eval.len

builtin stdSumInt, nkNodeKind("sum[int]", tNone):
  var r = 0
  for y in x:
    let ni = y.eval
    if ni.kind != nkInt: return nkError(erType, x, y, ni)
    r += ni.asInt
  r

builtin stdTreeEquals, nkNodeKind("tree ==", tNone):
  if x.len < 2: return nkError(erIllformedAst, x)
  if x[0].eval ==@ x[1].eval: cTrue else: cFalse

builtin stdDataEquals, nkNodeKind("data ==", tNone):
  if x.len < 2: return nkError(erIllformedAst, x)
  let
    na = x[0].eval
    nb = x[1].eval
  if na.kind != nb.kind: return nkError(erType, x, na, nb)
  if na.data == nb.data: cTrue else: cFalse

builtin godPanel, nkNodeKind("god panel", tNone):
  result = cNone
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
      let w = terminalWidth()
      var r = " ".repeat(indent).toRunes & result.toRunes & " ".repeat(w - result.runeLen - indent).toRunes
      result.add " ".repeat(w - result.runeLen - indent)
      result.insert " ".repeat(indent)
      if w > id.len + 2:
        r[^(id.len)..^1] = id.toRunes
        r[^(id.len + 1)] = " ".runeAt(0)
      result = $r


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
          echo s

      setCursorPos(0, h-3)
      echo()
      echo buffer.head(0)

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
 b set buffer to selected node
 k set kind to buffer node
 i insert child from buffer
 n create new node on buffer
 e eval selected node to buffer
 " read string to buffer
 I read int to buffer
 F read float to buffer
 [ move node up
 ] move node down
 h help
 ~ do something
 """
        for x in x[1]:
          if x.kind == nkString and x.len >= 2 and x[1].kind == nkString:
            echo " ", x.asString, " ", x[1].asString
          elif x.kind == nkString and x.len >= 1:
            echo x.asString

        discard getch()
      else:
        for x in x[1]:
          if x.kind == nkString and x.len >= 1 and x.asString == $k:
            discard x[0].eval

  except:
    echo getCurrentExceptionMsg()
    echo getStackTrace()

builtinNodes = @[
  tInt,
  tFloat,
  tNone,
  tString,

  nkNodeKind,
  nkString,
  nkInt,
  nkFloat,
  nkError,
  nkSeq,

  cNone,
  cTrue,
  cFalse,

  egStack,
  eLet,
  eIfStmt,
  eIf,
  eElse,
  eSeq,
  eWhile,
  eConcat,
  
  erIllformedAst,
  erIndex,
  erType,

  stdSumInt,
  stdDataEquals,

  stdPass,
  stdEval,
  stdGet,
  stdLen,
  stdInsert,
  stdDelete,
  stdTreeEquals,

  godPanel,
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
  except UsageError:
    stderr.writeLine getCurrentExceptionMsg()
    quit(1)
