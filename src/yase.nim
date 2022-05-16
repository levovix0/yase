import tables, sequtils
import cligen
import ast, eval3

let builtinNodes = [
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
]

proc serialize*(n: Node): string =
  var d: Table[Node, int32]
  var l: seq[Node]
  var i = 1.int32
  for i, n in builtinNodes:
    d[n] = -i.int32 - 1
  d[n] = 0
  l.add n

  for n in n.tree:
    if n notin d:
      d[n] = i
      inc i
      l.add n

  proc toString(n: Node): string =
    let a = @[d[n.kind], n.childs.len.int32, n.data.len.int32] & n.childs.mapit(d[it])
    result.setLen a.len * int32.sizeof + n.data.len
    copyMem(result[0].addr, a[0].unsafeaddr, a.len * int32.sizeof)
    if n.data.len > 0:
      copyMem(result[a.len * int32.sizeof].addr, n.data[0].unsafeaddr, n.data.len)

  for i, n in l:
    result.add n.toString

proc deserialize*(s: string): Node =
  var d: Table[int32, Node]
  for i, n in builtinNodes:
    d[-i.int32 - 1] = n

  var ni = 0.int32
  var i = 0.int32
  while i < s.len:
    var head: tuple[k, len, dlen: int32]
    if i + head.typeof.sizeof >= s.len: break
    copyMem(head.addr, s[i].unsafeaddr, head.typeof.sizeof)
    inc i, head.typeof.sizeof + head.len * int32.sizeof + head.dlen
    d[ni] = Node()
    inc ni

  ni = 0.int32
  i = 0.int32
  while i < s.len:
    var head: tuple[k, l, dlen: int32]
    if i + head.typeof.sizeof >= s.len: break
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

proc yase(input: string) =
  echo input.readFile.deserialize

when isMainModule:
  dispatch yase
