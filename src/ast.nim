import sequtils, strutils, strformat, macros, hashes

type
  Node* = ref object
    kind*: Node
    data*: seq[byte]
    childs*: seq[Node]


proc hash*(x: Node): Hash = x.data.len.hash


proc asInt*(x: Node): int =
  if x.data.len == int64.sizeof:
    cast[ptr int64](x.data[0].addr)[].int
  else: 0

proc asString*(x: Node): string =
  cast[string](x.data)

proc asBool*(x: Node): bool =
  x.data.len != 0

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
    ## store nothing
  tString* = Node()
    ## whole seq[byte] as string
  
  nkString* = nkNodeKind()


converter toNode*(s: string): Node = nkString{s}

nkNodeKind.childs = @[Node "node kind", tNone]
nkNodeKind.kind = nkNodeKind

nkString.childs = @[Node "string", tString]


var
  tInt* = Node()
    ## bytes as int if len == int64.sizeof, else 0
  tBool* = Node()
    ## true if len != 0, else false
  tFloat* = Node()
    ## bytes as float if len == float64.sizeof, else 0

  nkInt* = nkNodeKind("int", tInt)
  nkBool* = nkNodeKind("bool", tBool)
  nkFloat* = nkNodeKind("float", tFloat)

  nkNode* = nkNodeKind("node", tNone)
    ## handle child as data (==% with 2 nodes is always true if they have single child)
  nkSym* = nkNodeKind("sym", tNone)
    ## handle child equality (==@) as identity (==)

  nkError* = nkNodeKind("error", tNone)
  
  nkRecursion = nkNodeKind("recursion", tNone)
  
  ekNodeIndexOutOfBounds*: Node = "node index is out of bounds"
    ## index is out of bounds when getting a child node


converter toNode*(i: int): Node = nkInt{i.int64}

converter toNode*(v: bool): Node =
  if v: nkBool{[1'u8]}
  else: nkBool()

converter toNode*(i: float): Node = nkFloat{i.float64}


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
  if a.kind == nkSym and b.kind == nkSym:
    return a.childs == b.childs
  
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

proc `==<`*(a, b: Node): bool =
  ## a is sub of b
  ## true if begining of b data and childs is same to a
  ## for nkNode it don't check childs
  result = true
  if a.kind != b.kind or a.data != b.data[0..a.data.high] or a.childs.len > b.childs.len: return false
  if a.kind == nkNode: return true
  
  var stack = @[(a: a, b: b, h: (a: @[a], b: @[b]))]
  while stack.len != 0:
    let v = stack[^1]
    var na, nb: seq[Node]
    for (a, b) in zip(v.a.childs, v.b.childs):
      if a in v.h.a: continue
      if a.kind != b.kind or a.data != b.data[0..a.data.high] or a.childs.len > b.childs.len: return false
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


proc `&`*(a: Node, b: openarray[Node]): Node =
  a.kind(a.childs & b.toSeq)


macro `case`*(x: Node) =
  ## simple identity-based pattern matching
  result = newTree(nnkIfStmt)
  let selector = x[0]
  for it in x[1..^1]:
    case it.kind
    of nnkElse, nnkElifBranch, nnkElifExpr, nnkElseExpr:
      result.add it
    of nnkOfBranch:
      for jt in it[0..^2]:
        let cond = newCall("==", selector, jt)
        result.add newTree(nnkElifBranch, cond, it[^1])
    else:
      error "unexpected syntax", it


proc `$`*(x: Node): string =
  ## builtin node formating
  ## TODO: make algorithm not recursive and do not use copy
  if x.kind == nil: return "nil"
  if x.kind == nkRecursion: return "..."

  let x = copy(x, markRecursion=nkRecursion)
  result = x.kind[0].asString
  
  case x.kind[1]
  of tInt:
    result = &"{result} {x.asInt}"
  of tString:
    result.add " "
    result.addQuoted x.asString
  of tBool:
    result = &"{result} {x.asBool}"
  of tFloat:
    result = &"{result} {x.asFloat}"
  elif x.data.len != 0:
    result = &"{result} {x.data}"
  
  if x.childs.len != 0:
    let childs = x.childs.map(`$`).join("\n").indent(2)
    result.add ":\n"
    result.add childs
