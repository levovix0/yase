import tables, hashes, sequtils

{.experimental: "callOperator".}

type
  Module* = ref object
    root*: Node
    exported*: Table[string, Node]
    imports*: seq[tuple[file: string, instance: Module]]
    path*: string

  Node* = ref object
    kind*: Node
    childs*: seq[Node]
    params*: Table[Node, Node]
    data*: seq[byte]
    module* {.cursor.}: Module


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
