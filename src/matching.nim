import options
import ast

let
  maSameAs* = nkNodeKind("same as", tNone)
    ## structure:
    ##   node
  
  maSameKinds* = nkNodeKind("same kinds", tNone)
    ## structure:
    ##   node

  maStartsWith* = nkNodeKind("starts with", tNone)
    ## structure:
    ##   node


proc `>-`*(x, a: Node): bool =
  ## check if match
  case a.kind
  of maSameAs:
    return x ==@ a[0]
  of maStartsWith:
    return x ==< a[0]
  of maSameKinds:
    return x ==% a[0]

template `!>-`*(x, a): bool =
  not (x >- a)


proc `>>-`*(x, a: Node): Option[Node] =
  ## match and return some capture if match
  
