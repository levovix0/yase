import macros, algorithm, tables, sequtils
import fusion/matching, fusion/astdsl
import ast

# TODO:
# объявление функций с перегрузкой
# изменяемые переменные
# циклы

var
  nkNone*: Node = "none"
  nkIdent*: Node = "ident"

  nkCall*: Node = "call"
    ## structure:
    ##   callable
    ##   args (single node)
  
  nkSequence*: Node = "sequence"
    ## only last node eval of sequence is returned
  
  nkIfStmt*: Node = "if stmt"
    ## structure:
    ##   nkIfBranch...
    ##   [nkElseBranch]
  nkIfBranch*: Node = "if"
    ## structure:
    ##   condition
    ##   statement
  nkElseBranch*: Node = "else"
    ## structure:
    ##   statement

  nkLet*: Node = "let"
    ## let existance block
    ## expr returned
    ## structure:
    ##   ident
    ##   value (will be immediately evaled)
    ##   expr (can use same ident as value)
  
  nkExternLet*: Node = "extern let"
    ## let to call later
    ## can be called
    ## structure:
    ##   ident
    ##   expr (can use same ident as value)
  
  nkProc*: Node = "proc def"
    ## proc existance block
    ## let with overloading
    ## structure:
    ##   ident
    ##   signature (callable)
    ##   callable
    ##   expr (can call ident like function)

  ekIntDivisionByZero*: Node = "int division by zero"
  ekFailedToCall*: Node = "failed to call"
  ekIllformedAst*: Node = "illformed ast"


proc cleanError(x: Node): string =
  if x.kind != nkError: "ok"
  elif x[0].kind == ekFailedToCall:
    if x[1].kind == nkError: cleanError(x[1])
    else: "invalid signature"
  else: x[0].kind.asString


proc clean(x: Node): string =
  if x.kind == nkError: "error: " & cleanError(x)
  elif x.kind == nkInt: $x.asInt
  elif x.kind == nkString: $x.asString
  elif x.kind == nkBool: $x.asBool
  else: $x


macro node(x): Node =
  proc cv(x: NimNode): NimNode =
    case x
    of IntLit(intVal: @v):
      return quote do: Node(`v`)

    of StrLit(strVal: @v):
      return quote do: Node(`v`)

    of Ident(strVal: "false"):
      return quote do: Node(false)

    of Ident(strVal: "true"):
      return quote do: Node(true)

    of Ident(strVal: @name):
      return quote do: nkIdent(`name`)

    of AccQuoted[all @n]:
      return buildAst:
        call ident"nkIdent":
          for x in n: newLit $x

    of Par[@n], StmtList[@n]:
      return cv(n)
    
    of TupleConstr[all @n], Par[all @n]:
      return buildAst:
        call ident"nkTuple":
          for x in n: cv(x)
    
    of StmtList[all @n]:
      var r = newCall(ident"nkSequence")
      result = r
      for x in n:
        case x
        of LetSection[IdentDefs[@name, Empty(), @val]]:
          let nr = newCall(ident"nkSequence")
          r.add: buildAst:
            call ident"nkLet":
              cv(name)
              cv(val)
              nr
          r = nr

        of ProcDef[
          @name, Empty(), Empty(), FormalParams[Empty(), IdentDefs[@y, Empty(), Empty()]],
          Pragma[Ident(strVal: "where"), ExprColonExpr[@x, @signature]], Empty(), @body
        ]:
          let nr = newCall(ident"nkSequence")
          r.add: buildAst:
            call ident"nkProc":
              cv(name)
              call ident"nkExternLet":
                cv(x)
                cv(signature)
              call ident"nkExternLet":
                cv(y)
                cv(body)
              nr
          r = nr
        
        else: r.add cv(x)
    
    of Infix[Ident(strVal: "=>"), @a, @b]:
      let an = cv(a)
      let bn = cv(b)
      return quote do: nkExternLet(`an`, `bn`)

    of Call[@op, @n], Command[@op, @n], Infix[@op, @n], Prefix[@op, @n]:
      return buildAst:
        call ident"nkCall":
          cv(op)
          cv(n)
    
    of Call[@op, all @n], Command[@op, all @n], Infix[@op, all @n], Prefix[@op, all @n]:
      return buildAst:
        call ident"nkCall":
          cv(op)
          call ident"nkTuple":
            for x in n: cv(x)

    of DotExpr[@a, @op]:
      return buildAst:
        call ident"nkCall":
          cv(op)
          cv(a)
    
    of DotExpr[@a, @op, all @n]:
      return buildAst:
        call ident"nkCall":
          cv(op)
          call ident"nkTuple":
            cv(a)
            for x in n: cv(x)
    
    of BracketExpr[@a, all @n]:
      return buildAst:
        call ident"nkCall":
          call ident"nkIdent", newLit "[]"
          call ident"nkTuple":
            cv(a)
            for x in n: cv(x)
    
    of IfStmt[all @n]:
      return buildAst:
        call ident"nkIfStmt":
          for x in n:
            if x.kind == nnkElifBranch:
              call ident"nkIfBranch":
                cv(x[0])
                cv(x[1])
            elif x.kind == nnkElse:
              call ident"nkElseBranch":
                cv(x[0])
    
    else: error("unexpected syntax: " & x.treeRepr, x)
  
  cv(x)


type Builtin* = object
  ident*: Node
  signature*: Node
  f*: proc(args: Node): Node

var builtins: seq[Builtin]

template builtin(id, sign, body) =
  builtins.add Builtin(ident: node id, signature: node args => sign, f: proc(args {.inject.}: Node): Node = body)

builtin isTupleWith2Childs, true:
  args.kind == nkTuple and args.len == 2

builtin isTupleWithTupleAndInt, true:
  args.kind == nkTuple and args.len == 2 and args[0].kind == nkTuple and args[1].kind == nkInt

builtin `==`, args.isTupleWith2Childs:
  args[0] ==@ args[1]

builtin `==@`, args.isTupleWith2Childs:
  args[0] == args[1]

builtin `==%`, args.isTupleWith2Childs:
  args[0] ==% args[1]

builtin `==$`, args.isTupleWith2Childs:
  args[0] ==$ args[1]

builtin `==~`, args.isTupleWith2Childs:
  args[0] ==~ args[1]

builtin `[]`, args.isTupleWithTupleAndInt:
  args[0][args[1].asInt]


builtin node, true:
  nkNode args

builtin lit, args ==% node():
  args[0]

builtin kind, args ==% node():
  nkNode args[0].kind

builtin `[]`, args ==% (node(), int()):
  nkNode args[0][0][args[1].asInt]


builtin int, args ==% ():    0
builtin string, args ==% (): ""
builtin bool, args ==% ():   false
builtin error, args ==% ():  nkError()


builtin `-`, args ==% int():
  -args.asInt

builtin `+`, args ==% (int(), int()):
  args[0].asInt + args[1].asInt

builtin `-`, args ==% (int(), int()):
  args[0].asInt - args[1].asInt

builtin `*`, args ==% (int(), int()):
  args[0].asInt * args[1].asInt

builtin `div`, args ==% (int(), int()):
  let bi = args[1].asInt
  if bi == 0: return nkError(ekIntDivisionByZero())
  return Node `div`(args[0].asInt, bi)

builtin `mod`, args ==% (int(), int()):
  let bi = args[1].asInt
  if bi == 0: return nkError(ekIntDivisionByZero())
  return Node `mod`(args[0].asInt, bi)


builtin `&`, args ==% (string(), string()):
  args[0].asString & args[1].asString

builtin `$`, args ==% int():    $args.asInt
builtin `$`, args ==% string(): args
builtin `$`, args ==% bool():   $args.asBool


builtin `not`, args ==% bool():
  not args.asBool

builtin `and`, args ==% (bool(), bool()):
  args[0].asBool and args[1].asBool

builtin `or`, args ==% (bool(), bool()):
  args[0].asBool or args[1].asBool

builtin `xor`, args ==% (bool(), bool()):
  args[0].asBool xor args[1].asBool


builtin echo, args ==% string():
  echo args.asString
  nkNone()


builtin `()`, args ==~ () and args[0] ==~ node():
  if args[1].kind == nkNode: args[0][0](args[1][0])
  elif args[1].kind != nkTuple: args[0][0](args[1])
  else: args[0][0](args[1].childs.mapit(if it.kind == nkNode: it[0] else: it))


type Syms = object
  hasValue: seq[tuple[i, v: Node]]
  procs: seq[tuple[i, s, v: Node]]
  extern: seq[Node]

proc withHasValue(x: Syms; i, v: Node): Syms =
  result = x
  result.hasValue.add (i, v)

proc withProc(x: Syms; i, s, v: Node): Syms =
  result = x
  result.procs.add (i, s, v)

proc withExtern(x: Syms; i: Node): Syms =
  result = x
  result.extern.add i


proc link(x: Node): Node =
  ## make same idents in different scope different


proc eval*(x: Node, syms = Syms()): Node =
  ## eval node
  ## if error, result will be nkError
  result = x
  
  if x.kind == nkCall:
    let i = x[0].eval(syms)
    let args = x[1].eval(syms)

    # direct call
    if i.kind == nkExternLet:
      return i[1].eval(syms.withHasValue(i[0], args))

    # proc call
    for (id, s, v) in syms.procs.reversed:
      if id ==@ i and s[1].eval(syms.withHasValue(s[0], args)) ==@ true:
        return v[1].eval(syms.withHasValue(v[0], args))

    # builtin call
    for s in builtins.reversed:
      if s.ident ==@ i and s.signature[1].eval(syms.withHasValue(s.signature[0], args)) ==@ true:
        return s.f(args)
  
    # `()` call
    let args2 = nkTuple(i, args)
    for s in builtins.reversed:
      if s.ident ==@ nkIdent("()") and s.signature[1].eval(syms.withHasValue(s.signature[0], args2)) ==@ true:
        return s.f(args2)
    
    return nkCall(i, args)
  
  elif x.kind == nkSequence:
    for x in x:
      result = x.eval(syms)
  
  elif x.kind == nkTuple:
    result = nkTuple()
    for x in x:
      result.add x.eval(syms)
  
  elif x.kind == nkLet:
    let v = x[1].eval(syms)
    return x[2].eval(syms.withHasValue(x[0], v))
  
  elif x.kind == nkIdent:
    for i in syms.extern:
      if i ==@ x: return x
    for (i, v) in syms.hasValue.reversed:
      if i ==@ x: return v
    return x
  
  elif x.kind == nkIfStmt:
    for a in x:
      if a.kind == nkElseBranch: return a[0]
      elif a.kind == nkIfBranch:
        if a[0].eval(syms) ==@ true: return a[1].eval(syms)
      else: return nkError(ekIllformedAst(), x)
    result = nkNone()
  
  elif x.kind == nkExternLet:
    return nkExternLet(x[0], x[1].eval(syms.withExtern(x[0])))
  
  elif x.kind == nkProc:
    let s = x[1].eval(syms)
    if s.kind != nkExternLet: return nkError(ekIllformedAst(), nkProc(x[0], s, x[2], x[3]))
    let v = x[2].eval(syms)
    if v.kind != nkExternLet: return nkError(ekIllformedAst(), nkProc(x[0], s, v, x[3]))
    return x[3].eval(syms.withProc(x[0], s, v))


# optimize builtins signature
for x in builtins.mitems:
  x.signature = eval x.signature
  echo x.signature


template ecen(body) =
  echo clean eval node body

template den(body) =
  discard eval node body


ecen: 2 + 2 * 2
ecen: (2 + 2) * 2
ecen: 2 + 2 == 2 * 2
ecen: "Hello, " & "Мир!"
ecen: $2 & $3
ecen:
  let a = $3 & "2"
  if a == "32": "ok"
  else: "not ok"
ecen:
  let f = a => a * 4
  f(3)
ecen:
  let `!=` = x => not(x[0] == x[1])
  let `!=@` = x => not(x[0] ==@ x[1])
  node() != node(1) and node() ==% node(1) and node() !=@ node()
ecen:
  node("Привет")("это", "коструктор", "нод")
ecen:
  let f = (a => (b => a + b))#(2)(3)
  let f2 = b => f(b)(3)
  f2#(2)
# ecen:
  # proc f(x) {.where x: x ==% (int(), int()).} =
    # x[0] + x[1]
  # echo $f(2, 3)
  # echo $f(-7, 3)
  # echo $(not(f("a", "b") ==~ string()) and not(f("a", "b") ==~ int()))
