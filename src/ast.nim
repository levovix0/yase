import sequtils, strutils, strformat, macros, hashes, tables

type
  Node* = ref object
    kind*: Node
    data*: seq[byte]
    childs*: seq[Node]


proc hash*(x: Node): Hash = cast[int](x).hash


proc asInt*(x: Node): int =
  if x.data.len == int64.sizeof:
    cast[ptr int64](x.data[0].addr)[].int
  else: 0

proc asString*(x: Node): string =
  cast[string](x.data)

proc asFloat*(x: Node): float =
  if x.data.len == float64.sizeof:
    cast[ptr float64](x.data[0].addr)[].float
  else: 0


proc `{}`*(kind: Node, data: seq[byte]): Node =
  ## node constructor
  ## kind{data}
  Node(kind: kind, data: data)

proc `{}`*(kind: Node, data: openarray[byte]): Node =
  ## node constructor
  ## kind{data}
  Node(kind: kind, data: data.toSeq)

proc `{}`*[I](kind: Node, data: array[I, byte]): Node =
  ## node constructor
  ## kind{data}
  Node(kind: kind, data: @data)

proc `{}`*(kind: Node, data: string): Node =
  ## node constructor
  ## kind{data}
  Node(kind: kind, data: cast[seq[byte]](data))

proc `{}`*[T](kind: Node, data: T): Node =
  ## node constructor
  ## kind{data}
  Node(kind: kind, data: cast[ptr array[T.sizeof, byte]](data.unsafeaddr)[].`@`)

proc `()`*(kind: Node, childs: varargs[Node]): Node =
  ## node constructor
  ## kind(childs...)
  Node(kind: kind, childs: childs.toSeq)

proc `()`*(kind: Node, childs: varargs[Node], data: openarray[byte]): Node =
  ## node constructor
  ## kind(childs..., data=data)
  Node(kind: kind, data: data.toSeq, childs: childs.toSeq)

proc `()`*(kind: Node, childs: varargs[Node], data: seq[byte]): Node =
  ## node constructor
  ## kind(childs..., data=data)
  Node(kind: kind, data: data, childs: childs.toSeq)


proc toBytes*[T](x: T): seq[byte] = toSeq cast[ptr array[T.sizeof, byte]](x.unsafeaddr)[]
proc toBytes*(x: string): seq[byte] = cast[seq[byte]](x)


var
  nkNodeKind* = Node()
    ## node kind node
    ## structure:
    ##   name (nkString)
    ##   data type (t*)

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
  tFloat* = Node "store float64"

  nkInt* = nkNodeKind("int", tInt)
  nkFloat* = nkNodeKind("float", tFloat)

  nkError* = nkNodeKind("error", tNone)
    ## structure:
    ##   (optional) error kind
  
  nkRecursion = nkNodeKind("recursion", tNone)
    ## needed for implementation of $


converter toNode*(i: int): Node = nkInt{i.int64}
converter toNode*(i: float): Node = nkFloat{i.float64}


iterator items*(x: Node): Node =
  for x in x.childs: yield x

iterator pairs*(x: Node): (int, Node) =
  for i, x in x.childs: yield (i, x)

proc len*(x: Node): int =
  x.childs.len

proc `[]`*(x: Node, i: int): Node =
  x.childs[i]


iterator tree*(x: Node): Node =
  ## iterates over tree (excluding root)
  ## ignores if recursion
  var stack = @[(x: x, h: @[x])]
  while stack.len != 0:
    let v = stack[^1]
    let n = v.x.childs.filterit(it notin v.h)
    for x in n: yield x
    stack[^1..^1] = n.mapit((it, v.h & it))


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


proc `==@`*(a, b: Node): bool =
  ## same tree
  ## true if trees has same data and kinds for each node
  if a.kind != b.kind or a.data != b.data or a.childs.len != b.childs.len:
    return false
  
  result = true
  
  var stack = @[(a: a, b: b, h: (a: @[a], b: @[b]))]
  while stack.len != 0:
    let v = stack[^1]
    var na, nb: seq[Node]
    for (a, b) in zip(v.a.childs, v.b.childs):
      if a in v.h.a: continue
      if a.kind != b.kind or a.data != b.data or a.childs.len != b.childs.len: return false
      na.add a
      nb.add b
    stack[^1..^1] = zip(na, nb).mapit((it[0], it[1], (v.h.a & it[0], v.h.b & it[1])))


proc `&`*(a: Node, b: varargs[Node]): Node =
  a.kind(a.childs & b.toSeq)


proc `$`*(x: Node): string =
  ## builtin node formating
  ## TODO: make algorithm not recursive and do not use copy
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
    elif t == tFloat:
      result = &"{result} {x.asFloat}"
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
  var i = 1.int32
  for i, n in builtinNodes:
    d[n] = -i.int32 - 1
  d[n] = 0
  l.add n

  block builtin:
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

  for i, n in l:
    result.add n.toString

proc deserialize*(s: string, builtinNodes: openarray[Node]): Node =
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
