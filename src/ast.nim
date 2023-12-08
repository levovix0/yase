import tables, hashes, sequtils

{.experimental: "callOperator".}

type
  Node* = ref object
    kind*: Node
    childs*: seq[Node]
    params*: Table[Node, Node]
    data*: seq[byte]
    module* {.cursor.}: Module

  Module* = ref object
    root*: Node
    exported*: Table[string, Node]
    extendNodes*: seq[Node]
    imports*: seq[tuple[file: string, instance: Module]]
    path*: string


proc hash*(x: Node): Hash = cast[int](x).hash
proc hash*(x: Module): Hash = cast[int](x).hash


proc asInt*(x: Node): int =
  if x.data.len == int64.sizeof:
    cast[ptr int64](x.data[0].addr)[].int
  else: 0

proc asString*(x: Node): string =
  cast[string](x.data)


proc `{}`*(kind: Node, data: string): Node =
  Node(kind: kind, data: cast[seq[byte]](data))

proc `{}`*[T](kind: Node, data: T): Node =
  Node(kind: kind, data: cast[ptr array[T.sizeof, byte]](data.unsafeaddr)[].`@`)

proc `()`*(kind: Node, childs: varargs[Node]): Node =
  Node(kind: kind, childs: childs.toSeq)


proc toBytes*(x: string): seq[byte] = cast[seq[byte]](x)
proc toString*(x: seq[byte]): string = cast[string](x)


iterator items*(x: Node): Node =
  for x in x.childs: yield x

proc len*(x: Node): int = x.childs.len
proc `[]`*(x: Node, i: int): var Node = x.childs[i]
