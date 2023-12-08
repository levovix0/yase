import os, strutils, strformat, sequtils, terminal, unicode, tables, macros, hashes
import argparse
import ast, saveload1


var
  nkNodeKind = Node()

  tNone = Node()
  tString = Node()
  
  nkString = nkNodeKind()


converter toNode(s: string): Node = nkString{s}

nkNodeKind.childs = @[Node "node kind", tNone]
nkNodeKind.kind = nkNodeKind

nkString.childs = @[Node "string", tString]

tNone.kind = nkString
tNone.data = toBytes "store nothing"

tString.kind = nkString
tString.data = toBytes "store string"


var
  tInt = Node "store int64"

  nkInt = nkNodeKind("int", tInt)

  nkError = nkNodeKind("error", tNone)


converter toNode(i: int): Node = nkInt{i.int64}


proc serialize(n: Node, builtinNodes: openarray[Node]): string =
  var d: Table[Node, int32]
  var l: seq[Node]
  for i, n in builtinNodes:
    d[n] = -i.int32 - 1
  d[n] = 0
  l.add n

  var i = 1.int32
  block collectGraphToTable:
    var stack = @[(x: n, h: @[n])]
    while stack.len != 0:
      let v = stack[^1]
      let n = v.x.childs.filterit(it notin v.h and it notin builtinNodes)
      for x in n:
        if x notin d:
          d[x] = i
          inc i
          l.add x
      stack[^1..^1] = n.mapit((it, v.h & it))

  proc toString(n: Node): string =
    let a = @[d[n.kind], n.childs.len.int32, n.data.len.int32] & n.childs.mapit(d[it])
    result.setLen a.len * int32.sizeof + n.data.len
    copyMem(result[0].addr, a[0].unsafeaddr, a.len * int32.sizeof)
    if n.data.len > 0:
      copyMem(result[a.len * int32.sizeof].addr, n.data[0].unsafeaddr, n.data.len)

  result.add cast[array[16, char]](('y', 'a', 's', 'e', 0.uint32, 0.uint32, i.uint32)).join()

  for i, n in l:
    result.add n.toString


proc deserialize(s: string, builtinNodes: openarray[Node]): Node =
  if s.len < 16: return
  let head = cast[ptr tuple[magic: array[4, char], maj, min, n: uint32]](s[0].unsafeaddr)[]
  if head.magic != ['y', 'a', 's', 'e'] or head.maj != 0 or head.n == 0: return

  var d: Table[int32, Node]
  for i, n in builtinNodes:
    d[-i.int32 - 1] = n

  for i in 0.int32..<head.n.int32:
    d[i] = Node()
  
  var ni = 0.int32
  var i = 16.int32
  while i < s.len:
    var head: tuple[k, l, dlen: int32]
    if i + head.typeof.sizeof > s.len: break
    copyMem(head.addr, s[i].unsafeaddr, head.typeof.sizeof)
    inc i, head.typeof.sizeof
    d[ni].kind = d[head.k]
    if head.l > 0:
      d[ni].childs.setLen head.l
      var i2: seq[int32]
      i2.setLen head.l
      copyMem(i2[0].addr, s[i].unsafeaddr, head.l * int32.sizeof)
      for i, n in i2:
        d[ni].childs[i] = d[n]
      inc i, head.l * int32.sizeof
    if head.dlen > 0:
      d[ni].data.setLen head.dlen
      copyMem(d[ni].data[0].addr, s[i].unsafeaddr, head.dlen)
      inc i, head.dlen
    inc ni

  return d[0]


let
  cNone = Node "none"
  cTrue = Node "true"
  cFalse = Node "false"
  
  nkSeq = nkNodeKind("seq", tNone)

  eIf = nkNodeKind("if", tNone)
  eElse = nkNodeKind("else", tNone)

  erIllformedAst = Node "illformed ast"
  erType = Node "type error"
  erIndex = Node "index error"
  erParse = Node "parse error"
  erOs = Node "os error"

  egStack = Node "global eval stack"


var builtinNodes: seq[Node]
var builtins: Table[Node, proc(x: Node): Node]
var lets: Table[int, Table[Node, Node]]


converter toNode(x: bool): Node =
  if x: cTrue else: cFalse


proc eval(x: Node): Node =
  if x.kind in builtins:
    return builtins[x.kind](x)
  else:
    if x.kind.kind != nkNodeKind or x.kind.len < 3 or x.kind[2] == cNone: return x

    egStack.childs.add nkSeq(x.childs.map(eval))  # push args
    defer:
      lets.del egStack.len  # cleanup lets
      egStack.childs.setLen max(egStack.childs.high, 0)  # pop
    
    x.kind[2].eval

template builtin(name, node, body) {.dirty.} =
  let name = node
  builtins[name] = proc(x: Node): Node = body


builtin eSeq, nkNodeKind("eval sequence, return last", tNone):
  result = cNone
  for x in x.childs:
    result = x.eval

builtin eIfStmt, nkNodeKind("if statement", tNone):
  result = cNone
  for y in x:
    if y.kind == eIf:
      if y.len != 2:
        return nkError(erIllformedAst, x, y)
      let ny = y[0].eval
      if ny == cTrue:
        return y[1].eval
      elif ny != cFalse: return nkError(erType, x, y, ny)
    elif y.kind == eElse:
      if y.len != 1:
        return nkError(erIllformedAst, x, y)
      return y[0].eval

builtin eWhile, nkNodeKind("while", tNone):
  if x.len != 2:
    return nkError(erIllformedAst, x)
  result = cNone
  while x[0].eval == cTrue:
    result = x[1].eval

builtin eLet, nkNodeKind("let", tNone):
  if x.len != 1: return nkError(erIllformedAst, x)
  
  if egStack.len notin lets:
    lets[egStack.len] = {x[0]: x[0].eval}.toTable
  
  if x[0] notin lets[egStack.len]:
    lets[egStack.len][x[0]] = x[0].eval
  
  lets[egStack.len][x[0]]

builtin eLetLookup, nkNodeKind("let lookup", tNone):
  if x.len != 1: return nkError(erIllformedAst, x)
  
  var i = egStack.len - 1
  while i > 0:
    if i in lets:
      if x[0] in lets[i]:
        return lets[i][x[0]]
    dec i
  
  cNone

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
  for x in na: echo f(stdPass(x)).eval.asString
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
        elif x.kind.kind == nkString: "\"" & x.kind.asString & "\""
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
      var r = " ".repeat(indent).toRunes & result.toRunes & " ".repeat(max(0, w - result.runeLen - indent)).toRunes
      result.add " ".repeat(max(0, w - result.runeLen - indent))
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
    echo getCurrentException().getStackTrace

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

macro makeBuiltin2(body: untyped): auto =
  result = nnkTableConstr.newTree(body.mapit(
    if it.kind == nnkCommand:
      nnkExprColonExpr.newTree(
        it[1],
        it[0]
      )
    else:
      nnkExprColonExpr.newTree(
        newLit $it,
        it
      )
  ))

var builtinNodes2 = makeBuiltin2:
  tInt
  tNone
  tString

  nkNodeKind "nk"
  nkString "s"
  nkInt "i"
  nkError "e"
  nkSeq "a"

  cNone "n"
  cTrue "t"
  cFalse "f"

  egStack
  eLet
  eIfStmt
  eIf
  eElse
  eSeq
  eWhile
  eLetLookup

  erIllformedAst
  erIndex
  erType
  erParse
  erOs

  stdSumInt
  stdDataEquals
  stdSetData
  stdPass
  stdEval
  stdGet
  stdLen
  stdKind
  stdInsert
  stdDelete
  stdCopyNode
  stdLessInt
  stdAskSelect
  stdSetKind
  stdIdentity
  stdReadLine
  stdEcho
  stdDataLen
  stdDataGet
  stdDataInsert
  stdDataDelete
  stdParseInt
  stdFormatInt
  stdIntToByte
  stdByteToInt
  stdWriteFile
  stdReadFile
  stdSerializeNode
  stdDeserializeNode

  godPanel

var builtinModule = Module(
  path: "builtin",
  exported: builtinNodes2.toTable,
)

for (_, x) in builtinNodes2:
  x.module = builtinModule


var yase = newParser:
  help("Yet another self-editor")
  arg("input", default=some"main.yase", help="eval file")
  run:
    var input = opts.input
    if not input.fileExists and (input & ".yase").fileExists:
      input = input & ".yase"
    # discard input.readFile.deserialize(builtinNodes).eval

    let m = load1(
      "lib/editor.yase".readFile,
      @[
        builtinModule,
      ],
      default_readModule(),
      "std/editor.yase",
    ).m

    discard eval m.root

    # writeFile("lib/editor2.yase", save1(m))

when isMainModule:
  try:
    run yase
  except UsageError:
    stderr.writeLine getCurrentExceptionMsg()
    quit(1)
