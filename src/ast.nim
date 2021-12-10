import sequtils, strutils, strformat

type
  Node* = ref object
    kind*: Node
    data*: seq[byte]
    childs*: seq[Node]


proc asInt*(x: Node): int =
  if x.data.len == int64.sizeof:
    cast[ptr int64](x.data[0].addr)[].int
  else: 0

proc asString*(x: Node): string =
  cast[string](x.data)

proc asBool*(x: Node): bool =
  x.data.len != 0


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


var nkString* = Node(data: cast[seq[byte]]("string"))
nkString.kind = nkString

converter toNode*(s: string): Node = nkString(data=cast[seq[byte]](s))

var
  nkInt*: Node = "int"
  nkBool*: Node = "bool"
  nkTuple*: Node = "tuple"

  nkNode*: Node = "node"
    ## handle child as data (==% with 2 nodes is always true if they have single child)
  nkSym*: Node = "sym"
    ## handle child equality (==@) as identity (==)

  nkError*: Node = "error"
  
  nkRecursion: Node = "recursion"
  
  ekNodeIndexOutOfBounds*: Node = "node index is out of bounds"
    ## index is out of bounds when getting a child node

converter toNode*(i: int): Node =
  var i = i.int64
  nkInt(data=cast[ptr array[int64.sizeof, byte]](i.addr)[])

converter toNode*(v: bool): Node =
  if v: nkBool(data=[1'u8])
  else: nkBool()


proc contains[A, B](x: HSlice[A, B], i: BackwardsIndex): bool =
  (i.int - 1) in x

proc delete(x: var seq, i: BackwardsIndex) =
  x.delete x.len - i


proc `[]`*(x: Node, i: int|BackwardsIndex): Node =
  ## returns child of node
  ## if index is out of bounds, returns error ekNodeIndexOutOfBounds
  if i notin 0..x.childs.high: nkError(ekNodeIndexOutOfBounds())
  else: x.childs[i]

proc `[]=`*(x: Node, i: int|BackwardsIndex, v: Node) =
  ## sets child of node
  ## if index is out of bounds, do nothing
  if i notin 0..x.childs.high: return
  x.childs[i] = v

proc add*(x: Node, n: Node) =
  ## adds child to node
  x.childs.add n

proc insert*(x: Node, n: Node, i: int|BackwardsIndex) =
  ## insert child to node
  x.childs.insert n

proc delete*(x: Node, i: int|BackwardsIndex) =
  ## insert child of node
  ## if index is out of bounds, do nothing
  if i notin 0..x.childs.high: return
  x.childs.delete i


iterator items*(x: Node): Node =
  for x in x.childs: yield x

iterator pairs*(x: Node): (int, Node) =
  for i, x in x.childs: yield (i, x)


proc len*(x: Node): int =
  x.childs.len


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
  result = true
  if a.kind != b.kind or a.data != b.data or a.childs.len != b.childs.len: return false
  
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

proc `==%`*(a, b: Node): bool =
  ## same tree structure
  ## true if trees has same kinds for each node
  ## for nkNode it don't check childs
  result = true
  if a.kind != b.kind or a.childs.len != b.childs.len: return false
  if a.kind == nkNode: return true
  
  var stack = @[(a: a, b: b, h: (a: @[a], b: @[b]))]
  while stack.len != 0:
    let v = stack[^1]
    var na, nb: seq[Node]
    for (a, b) in zip(v.a.childs, v.b.childs):
      if a in v.h.a: continue
      if a.kind != b.kind or a.childs.len != b.childs.len: return false
      if a.kind == nkNode: continue
      na.add a
      nb.add b
    stack[^1..^1] = zip(na, nb).mapit((it[0], it[1], (v.h.a & it[0], v.h.b & it[1])))

proc `==$`*(a, b: Node): bool =
  ## same node
  ## true if nodes has same data and kind
  a.kind == b.kind and a.data == b.data

proc `==~`*(a, b: Node): bool =
  ## same node kind
  ## true if nodes has same kind
  a.kind == b.kind


proc `$`*(x: Node): string =
  ## builtin node formating
  ## TODO: make algorithm not recursive and do not use copy
  if x.kind == nil: return "nil"
  if x.kind == nkRecursion: return "..."

  let x = copy(x, markRecursion=nkRecursion)
  result = x.kind.asString
  
  if x.kind == nkInt:
    result = &"{result} {x.asInt}"
  elif x.kind == nkString:
    result.add " "
    result.addQuoted x.asString
  elif x.kind == nkBool:
    result = &"{result} {x.asBool}"
  elif x.data.len != 0:
    result = &"{result} {x.data}"
  
  if x.childs.len != 0:
    let childs = x.childs.map(`$`).join("\n").indent(2)
    result.add ":\n"
    result.add childs
