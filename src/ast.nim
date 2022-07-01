import sequtils, strutils, strformat, macros, hashes, tables

type
  Node* = ref object
    kind*: Node
    childs*: seq[Node]
    data*: seq[byte]


proc hash*(x: Node): Hash = cast[int](x).hash


proc asInt*(x: Node): int =
  if x.data.len == int64.sizeof:
    cast[ptr int64](x.data[0].addr)[].int
  else: 0

proc asString*(x: Node): string =
  cast[string](x.data)


proc `{}`*(kind: Node, data: seq[byte]): Node =
  Node(kind: kind, data: data)

proc `{}`*(kind: Node, data: openarray[byte]): Node =
  Node(kind: kind, data: data.toSeq)

proc `{}`*[I](kind: Node, data: array[I, byte]): Node =
  Node(kind: kind, data: @data)

proc `{}`*(kind: Node, data: string): Node =
  Node(kind: kind, data: cast[seq[byte]](data))

proc `{}`*[T](kind: Node, data: T): Node =
  Node(kind: kind, data: cast[ptr array[T.sizeof, byte]](data.unsafeaddr)[].`@`)

proc `()`*(kind: Node, childs: varargs[Node]): Node =
  Node(kind: kind, childs: childs.toSeq)

proc `()`*(kind: Node, childs: varargs[Node], data: openarray[byte]): Node =
  Node(kind: kind, data: data.toSeq, childs: childs.toSeq)

proc `()`*(kind: Node, childs: varargs[Node], data: seq[byte]): Node =
  Node(kind: kind, data: data, childs: childs.toSeq)


proc toBytes*[T](x: T): seq[byte] = toSeq cast[ptr array[T.sizeof, byte]](x.unsafeaddr)[]
proc toBytes*(x: string): seq[byte] = cast[seq[byte]](x)


var
  nkNodeKind* = Node()

  tNone* = Node()
  tString* = Node()
  
  nkString* = nkNodeKind()


converter toNode*(s: string): Node = nkString{s}

nkNodeKind.childs = @[Node "node kind", tNone]
nkNodeKind.kind = nkNodeKind

nkString.childs = @[Node "string", tString]

tNone.kind = nkString
tNone.data = toBytes "store nothing"

tString.kind = nkString
tString.data = toBytes "store string"


var
  tInt* = Node "store int64"

  nkInt* = nkNodeKind("int", tInt)

  nkError* = nkNodeKind("error", tNone)
    ## structure:
    ##   (optional) error kind
  
  nkRecursion = nkNodeKind("recursion", tNone)
    ## needed for implementation of $


converter toNode*(i: int): Node = nkInt{i.int64}


iterator items*(x: Node): Node =
  for x in x.childs: yield x

iterator pairs*(x: Node): (int, Node) =
  for i, x in x.childs: yield (i, x)

proc len*(x: Node): int = x.childs.len
proc `[]`*(x: Node, i: int): var Node = x.childs[i]


proc copy*(x: Node, markRecursion: Node = nil): Node =
  ## copies node and all its children (not kinds)
  ## if markRecursion is not nil, adds this mark node before recursion
  proc copySingle(x: Node): Node =
    x.kind(data=x.data)
  
  result = copySingle x
  var stack = @[(x: x, n: result, h: (x: @[x], n: @[result]))]
  while stack.len != 0:
    let v = stack[^1]
    var nx, nn: seq[Node]
    for x in v.x.childs:
      let xi = v.h.x.find(x)
      if xi != -1:
        if markRecursion == nil:
          v.n.childs.add v.h.n
        else:
          v.n.childs.add markRecursion(v.h.n)
      else:
        let a = copySingle(x)
        v.n.childs.add a
        nn.add a
        nx.add x
    stack[^1..^1] = zip(nx, nn).mapit((it[0], it[1], (v.h.x & it[0], v.h.n & it[1])))


proc `$`*(x: Node): string =
  ## builtin node formating
  if x == nil: return "nil"
  if x.kind == nil: return "nil"
  if x.kind == nkRecursion: return "..."

  let x = copy(x, markRecursion=nkRecursion)
  result = if x.kind.childs.len > 0: x.kind.childs[0].asString else: "undocumented Node"

  if x.kind.childs.len > 1:
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
  
  if x.childs.len != 0:
    let childs = x.childs.map(`$`).join("\n").indent(2)
    result.add ":\n"
    result.add childs

proc serialize*(n: Node, builtinNodes: openarray[Node]): string =
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

proc deserialize*(s: string, builtinNodes: openarray[Node]): Node =
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
