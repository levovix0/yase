import os, strutils, strformat, sequtils, terminal, unicode
import argparse
import ast, eval

var builtinNodes: seq[Node]

let
  erIndex* = Node "index error"
  erParse* = Node "parse error"
  erOs* = Node "os error"

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

builtin stdDataEquals, nkNodeKind("data ==", tNone):
  if x.len < 2: return nkError(erIllformedAst, x)
  let
    na = x[0].eval
    nb = x[1].eval
  if na.kind != nb.kind: return nkError(erType, x, na, nb)
  if na.data == nb.data: cTrue else: cFalse

builtin stdSetData, nkNodeKind("set data", tNone):
  if x.len < 2: return nkError(erIllformedAst, x)
  result = cNone
  let
    na = x[0].eval
    nb = x[1].eval
  if na.kind != nb.kind: return nkError(erType, x, na, nb)
  na.data = nb.data

builtin stdCopyNode, nkNodeKind("copy node", tNone):
  if x.len < 1: return nkError(erIllformedAst, x)
  let n = x[0].eval
  Node(kind: n.kind, childs: n.childs, data: n.data)

builtin stdLessInt, nkNodeKind("<[int]", tNone):
  if x.len < 2: return nkError(erIllformedAst, x)
  let
    na = x[0].eval
    nb = x[1].eval
  if na.kind != nkInt or nb.kind != nkInt: return nkError(erType, x, na, nb)
  if na.asInt < nb.asInt: cTrue else: cFalse

builtin stdSetKind, nkNodeKind("set kind", tNone):
  if x.len < 2: return nkError(erIllformedAst, x)
  result = cNone
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

builtin stdReadLine, nkNodeKind("read line", tNone):
  stdin.readLine

builtin stdEcho, nkNodeKind("write", tNone):
  if x.len < 1: return nkError(erIllformedAst, x)
  result = cNone
  stdout.write x[0].eval.asString

builtin stdDataLen, nkNodeKind("data len", tNone):
  if x.len < 1: return nkError(erIllformedAst, x)
  x[0].eval.data.len

builtin stdDataGet, nkNodeKind("get data", tNone):
  if x.len < 3: return nkError(erIllformedAst, x)
  let n = x[0].eval
  let na = x[1].eval
  if na.kind != nkInt: return nkError(erType, x, na)
  let nb = x[2].eval
  if nb.kind != nkInt: return nkError(erType, x, nb)
  Node(kind: nkSeq, data: n.data[na.asInt.min(n.data.high).max(0)..nb.asInt.min(n.data.high).max(-1)])

builtin stdDataInsert, nkNodeKind("insert data", tNone):
  if x.len < 3: return nkError(erIllformedAst, x)
  result = cNone
  let n = x[0].eval
  let nb = x[1].eval
  let ni = x[2].eval
  if ni.kind != nkInt: return nkError(erType, x, ni)
  let i = ni.asInt
  n.data.insert nb.data, i

builtin stdDataDelete, nkNodeKind("delete data", tNone):
  if x.len < 3: return nkError(erIllformedAst, x)
  result = cNone
  let n = x[0].eval
  let na = x[1].eval
  if na.kind != nkInt: return nkError(erType, x, na)
  let nb = x[2].eval
  if nb.kind != nkInt: return nkError(erType, x, nb)
  n.data.delete na.asInt.min(n.data.high).max(0)..nb.asInt.min(n.data.high).max(-1)

builtin stdParseInt, nkNodeKind("parse[int]", tNone):
  if x.len < 1: return nkError(erIllformedAst, x)
  let n = x[0].eval
  if n.kind != nkString: return nkError(erType, x, n)
  try: Node n.asString.parseInt
  except ValueError: nkError(erParse, x, n)

builtin stdFormatInt, nkNodeKind("format[int]", tNone):
  if x.len < 1: return nkError(erIllformedAst, x)
  let n = x[0].eval
  if n.kind != nkInt: return nkError(erType, x, n)
  $n.asInt

builtin stdIntToByte, nkNodeKind("int to byte", tNone):
  if x.len < 1: return nkError(erIllformedAst, x)
  let n = x[0].eval
  if n.kind != nkInt: return nkError(erType, x, n)
  Node(kind: nkSeq, data: @[n.asInt.byte])

builtin stdByteToInt, nkNodeKind("bytes to ints", tNone):
  if x.len < 1: return nkError(erIllformedAst, x)
  let n = x[0].eval
  nkSeq(n.data.mapit(it.int.Node))

builtin stdWriteFile, nkNodeKind("write file", tNone):
  if x.len < 2: return nkError(erIllformedAst, x)
  result = cNone
  let
    nf = x[0].eval
    nd = x[1].eval
  if nf.kind != nkString: return nkError(erType, x, nf)
  let file = nf.asString
  createDir(file.splitPath.head)
  writeFile(file, nd.data)

builtin stdReadFile, nkNodeKind("read file", tNone):
  if x.len < 1: return nkError(erIllformedAst, x)
  let nf = x[0].eval
  if nf.kind != nkString: return nkError(erType, x, nf)
  let file = nf.asString
  try: Node readFile(file)
  except: nkError(erOs, x, nf, getCurrentExceptionMsg())

builtin stdSerializeNode, nkNodeKind("serialize node", tNone):
  if x.len < 1: return nkError(erIllformedAst, x)
  let n = x[0].eval
  n.serialize(builtinNodes)

builtin stdDeserializeNode, nkNodeKind("deserialize node", tNone):
  if x.len < 1: return nkError(erIllformedAst, x)
  let n = x[0].eval
  try: cast[string](n.data).deserialize(builtinNodes)
  except: nkError(erParse, x, n, getCurrentExceptionMsg())


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
    if x[2][2].kind != nkString or x[2][2].asString != "buffer": return nkError(erIllformedAst, x)
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
    
    let nCurrentNode = x[2][0]
    template currentNode: Node =
      if nCurrentNode.len < 1: nil.Node else: nCurrentNode[0]

    let i = x[2][1]

    let nBuffer = x[2][2]
    template buffer: Node =
      if nBuffer.len < 1: nil.Node else: nBuffer[0]

    while true:
      drawUi(currentNode, buffer, i.asInt)

      let k = getch()
      case k
      of 'q':
        eraseScreen()
        setCursorPos(0, 0)
        break
      of '~': discard
      of 'h':
        eraseScreen()
        setCursorPos(0, 0)
        echo """
 h help
 ~ do magic
 q quit"""
        for x in x[1]:
          if x.kind == nkString and x.len >= 2 and x[1].kind == nkString:
            echo " ", x.asString, " ", x[1].asString
          elif x.kind == nkString and x.len >= 1:
            echo " ", x.asString

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
  tNone,
  tString,

  nkNodeKind,
  nkString,
  nkInt,
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
  eLetLookup,

  erIllformedAst,
  erIndex,
  erType,
  erParse,
  erOs,

  stdSumInt,
  stdDataEquals,
  stdSetData,
  stdPass,
  stdEval,
  stdGet,
  stdLen,
  stdKind,
  stdInsert,
  stdDelete,
  stdCopyNode,
  stdLessInt,
  stdAskSelect,
  stdSetKind,
  stdIdentity,
  stdReadLine,
  stdEcho,
  stdDataLen,
  stdDataGet,
  stdDataInsert,
  stdDataDelete,
  stdParseInt,
  stdFormatInt,
  stdIntToByte,
  stdByteToInt,
  stdWriteFile,
  stdReadFile,
  stdSerializeNode,
  stdDeserializeNode,

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
