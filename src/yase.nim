import os, strutils, strformat, sequtils, terminal, unicode
import argparse
import ast, eval

var builtinNodes: seq[Node]

let
  erIndex* = Node "index error"

builtin stdInsert, nkNodeKind("insert child", tNone):
  if x.len < 3: return nkError(erIllformedAst, x)
  result = cNone
  let ni = x[1].eval
  if ni.kind != nkInt: return nkError(erType, x, ni)
  let i = ni.asInt
  x[0].eval.childs.insert x[2].eval, i

builtin stdDelete, nkNodeKind("delete child", tNone):
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
  if x.len == 1:
    if 0 notin 0..<nx.len: return nkError(erIndex, x, nx, 0)
    return nx[0]
  
  let ni = x[1].eval
  if ni.kind != nkInt: return nkError(erType, x, ni)
  let i = ni.asInt
  if i notin 0..<nx.len: return nkError(erIndex, x, nx, ni)
  nx[i]

builtin stdLen, nkNodeKind("len", tNone):
  if x.len < 1: return nkError(erIllformedAst, x)
  x[0].eval.len

builtin stdKind, nkNodeKind("kind", tNone):
  if x.len < 1: return nkError(erIllformedAst, x)
  x[0].eval.kind

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

builtin stdSetData, nkNodeKind("set data", tNone):
  if x.len < 2: return nkError(erIllformedAst, x)
  let
    na = x[0].eval
    nb = x[1].eval
  if na.kind != nb.kind: return nkError(erType, x, na, nb)
  na.data = nb.data

builtin stdCopyNode, nkNodeKind("copy node", tNone):
  if x.len < 1: return nkError(erIllformedAst, x)
  let n = x[0].eval
  result = Node(kind: n.kind, childs: n.childs, data: n.data)

builtin stdLessInt, nkNodeKind("<[int]", tNone):
  if x.len < 2: return nkError(erIllformedAst, x)
  let
    na = x[0].eval
    nb = x[1].eval
  if na.kind != nkInt or nb.kind != nkInt: return nkError(erType, x, na, nb)
  if na.asInt < nb.asInt: cTrue else: cFalse

builtin stdSetKind, nkNodeKind("set kind", tNone):
  if x.len < 2: return nkError(erIllformedAst, x)
  let
    na = x[0].eval
    nb = x[1].eval
  na.kind = nb

builtin stdIdentity, nkNodeKind("is identical", tNone):
  if x.len < 2: return nkError(erIllformedAst, x)
  let
    na = x[0].eval
    nb = x[1].eval
  na == nb


builtin stdAskSelect, nkNodeKind("ask select", tNone):
  if x.len < 2: return nkError(erIllformedAst, x)
  let
    na = x[0].eval
    f = x[1].eval
  if f.kind != nkNodeKind: return nkError(erType, x, f)
  if na.len == 0: return nkError(erIndex, x, na, 0)
  for x in na: echo f(stdPass(x)).eval
  let s = na.childs.mapit(f(stdPass(it)).eval.asString)

  proc drawUi(s: seq[string], i: int) =
    eraseScreen()
    setCursorPos(0, 0)
    let h = terminalHeight()
    var s = s
    var i = i
    if s.len >= h-3:
      let start = (i-(h div 5)).max(0).min(s.high)
      s = s[start .. (start + h-3).min(s.high)]
      i -= start

    for i2, s in s:
      if i2 == i:
        styledEcho(bgWhite, fgBlack, s, resetStyle)
      else:
        echo s
  
  var i = 0
  while true:
    drawUi(s, i)
    case getch()
    of 's':
      inc i
      if i > s.high: i = 0
    of 'w':
      dec i
      if i < 0: i = s.high
    of 13.char:
      return i
    of 27.char:
      return -1
    else: discard


builtin godPanel, nkNodeKind("god panel", tNone):
  result = cNone
  try:
    if x.len < 3 or x[2].len < 4: return nkError(erIllformedAst, x)
    if x[2][0].kind != nkString or x[2][0].asString != "current node": return nkError(erIllformedAst, x)
    if x[2][1].kind != nkInt: return nkError(erIllformedAst, x)
    if x[2][2].kind != nkString or x[2][2].asString != "path": return nkError(erIllformedAst, x)
    if x[2][3].kind != nkString or x[2][3].asString != "buffer": return nkError(erIllformedAst, x)
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
    
    let nCurrentNode = x[2][0]
    template currentNode: Node =
      if nCurrentNode.len < 1: nil.Node else: nCurrentNode[0]
    template setCurrentNode(v: Node) =
      if nCurrentNode.len < 1: nCurrentNode.childs.add v
      else: nCurrentNode.childs[0] = v

    let i = x[2][1]

    let path = x[2][2]

    let nBuffer = x[2][3]
    template buffer: Node =
      if nBuffer.len < 1: nil.Node else: nBuffer[0]
    template setBuffer(v: Node) =
      if nBuffer.len < 1: nBuffer.childs.add v
      else: nBuffer.childs[0] = v

    template selectedNode: Node =
      if i == 0: currentNode else: currentNode()[i.asInt-1]

    while true:
      drawUi(currentNode, buffer, i.asInt)

      let k = getch()
      case k
      of 'q':
        eraseScreen()
        setCursorPos(0, 0)
        break
      of 's':
        i.data = toBytes (i.asInt + 1)
        if i.asInt > currentNode.len: i.data = toBytes 0
      of 'w':
        i.data = toBytes (i.asInt - 1)
        if i.asInt < 0: i.data = toBytes currentNode.len
      of 'a':
        while path.len > 0 and path.childs[^1].len < 1:
          path.childs.del path.childs.high
        if path.len == 0: continue
        let (ni, nc) = (path.childs[^1], path.childs[^1][0])
        setCurrentNode nc
        if ni.kind == nkInt:
          i.data = ni.data
        else:
          i.data = toBytes 0
        path.childs.del path.childs.high
        i.data = toBytes i.asInt.max(0).min(currentNode.childs.len)
      of 'd':
        if i.asInt == 0: continue
        path.childs.add i.asInt
        path.childs[^1].childs.add currentNode
        setCurrentNode currentNode[i.asInt-1]
        i.data = toBytes 0
      of 'D':
        if i == 0: continue
        currentNode.childs.delete i.asInt-1
        if i.asInt > currentNode.len: i.data = toBytes currentNode.len
      of 'S':
        writeFile "main.yase", root.serialize(builtinNodes)
      of 'b':
        setBuffer selectedNode
      of 'k':
        selectedNode.kind = buffer
      of 'i':
        if buffer == nil: continue
        currentNode.childs.insert buffer, i.asInt
        i.data = toBytes (i.asInt + 1)
      of 'e':
        setBuffer eval selectedNode
      of '"':
        setBuffer stdin.readLine
      of 'I':
        setBuffer:
          try: parseInt stdin.readLine
          except: 0
      of 'F':
        setBuffer:
          try: parseFloat stdin.readLine
          except: 0
      of '~':
        currentNode.childs.insert stdKind, i.asInt
      of 'h':
        eraseScreen()
        setCursorPos(0, 0)
        echo """
 h help
 q quit
 ~ do magic

 s move down
 w move up
 a move to parent
 d move to child
 D delete child
 S save
 b set buffer to selected node
 k set kind to buffer node
 i insert child from buffer
 e eval selected node to buffer
 " read string to buffer
 I read int to buffer
 F read float to buffer
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
            break

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
  
  stdSetData,

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

  stdCopyNode,
  stdLessInt,
  stdAskSelect,
  stdSetKind,
  stdIdentity,

  eLetLookup,
  
  stdKind,
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
