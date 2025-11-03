import
    std/sets,
    std/json,
    std/sugar,
    std/macros,
    std/tables,
    std/marshal,
    std/sequtils,
    std/macrocache,
    std/typetraits,
    std/strformat,
    std/algorithm,
    std/strutils,
    std/options,
    std/locks,
    std/os


export
    sets,
    json,
    sugar,
    macros,
    tables,
    marshal,
    sequtils,
    typetraits,
    strformat,
    algorithm,
    strutils,
    options,
    locks,
    os

type
    TypeID* = uint16

    Class* {. inheritable .} = ref object
    Closure* = distinct auto

    TypedPointer* = ref object
        data: pointer
        name: string
        size: int

    InitHook = proc(): void {. closure, gcsafe .}
    TrapHook = proc(this: Facet): TypedPointer {. nimcall .}
    TreeCall = proc(node: NimNode, ctx: NimNode): NimNode

    Storage* = object
        instances*: Table[TypeId, ref RootObj]
        lock*: Lock

    App* = ref object of Class
        config*: Config
        store*: Storage

    Config* = ref object of Class
        data*: seq[Facet]

    Facet* = ref object of Class
        app*: App
        class*: TypeID
        scope*: TypeID
        call: TypedPointer

    Hook* = ref object of Facet
        init*: bool = false

    Plan* = tuple[
        realm: string,
        scope: TypeID,
        class: TypeID,
        index: int
    ]

const
    nextTypeID = CacheCounter("mininim.types")

var
    facetPlans {. compileTime .} = newSeq[Plan]()
    facetNodes {. compileTime .} = newSeq[NimNode]()
let
    config* = Config()

proc init*(this: auto) =
    discard

proc `%`*(p: App): JsonNode =
    result = newJInt(cast[int](addr p))

proc `%`*(p: pointer): JsonNode =
    result = newJInt(cast[int](p))

proc `%`*(t: tuple): JsonNode =
  result = newJObject()
  for k, v in t.fieldPairs():
    result[k] = % v

proc type*(self: typedesc): TypeID =
    result = self.typeID

proc trap*[V](self: typedesc, value: V): TypedPointer =
    var
        head: pointer

    let
        name = name(V)
        size = sizeof(V)

    when V is not ref:
        head = alloc(size)

        copyMem(head, unsafeAddr value, size)

    else:
        head = addr value

    result = TypedPointer(
        data: head,
        name: name,
        size: size,
    )

    when defined(debug):
        echo fmt "trapped[{align($result.size, 3, '0')}]: {result.name} @ {$cast[int](result.data)}"

proc get*(self: typedesc, this: TypedPointer): ptr self {. gcsafe .} =
#    if $T != this.name:
#        raise newException(CatchableError, "Type mismatch: expected one of " & this.name & ", got " & $T)
#    if this.kind != Closure and sizeof(T) != this.size:
#        raise newException(CatchableError, "Size mismatch: expected " & $this.size & ", got " & $sizeof(T))

    # Handle cloosure

    result = cast[ptr self](this.data)

proc value*(self: typedesc, this: TypedPointer): self {. gcsafe .}=
    let
        p = self.get(this)

    when defined(debug):
        if p == nil:
            echo fmt "missing[{align($this.size, 3, '0')}]: {this.name} @ {$cast[int](this.data)}"

    when self is proc and sizeof(self) > sizeof(int):
        result = default(self)

        copyMem(addr result, p, sizeof(self))

    result = cast[self](p[])

    when defined(debug):
        echo fmt "secured[{align($this.size, 3, '0')}]: {this.name} @ {$cast[int](this.data)}"

proc talkTree(node: NimNode, call: TreeCall, ctx: NimNode = nil): NimNode {. compileTime .} =
    result = call(node, ctx)

    if result.len == 0 and result.kind == node.kind:
        for i, child in node:
            result.add(talkTree(child, call, node))

converter typeID*(T: typedesc): TypeID =
    const id = nextTypeID.value

    static:
        when defined(debug):
            echo fmt "Registering {$T} with id {id}"

        inc nextTypeID

    result = id.TypeID

template static*() {. pragma .}
template mutator*() {. pragma .}

macro begin*(scope: typedesc, body: untyped) =
    var
        parent: string

    let
        target = scope.strVal

    if scope.getImpl[2][0].len > 0 and scope.getImpl[2][0][1].len > 0:
        parent = scope.getImpl[2][0][1][0].strVal
    else:
        parent = "Class"

    for i1, c1 in body:
        if c1.kind in [nnkMethodDef, nnkProcDef, nnkIteratorDef]:
            var stc = false
            var cpy = true
            var abs = true

            for i2, c2 in c1:
                if c2.kind == nnkPragma:
                    for i3, c3 in c2:
                        if c3.kind == nnkIdent:
                            if c3.strVal == "static":
                                stc = true
                            if c3.strVal == "mutator":
                                cpy = false

                elif c2.kind == nnkFormalParams:
                    if c2[0].kind == nnkIdent:
                        if c2[0].strVal == "self":
                            c2[0] = newIdentNode(target)

                    c2.insert(1, newNimNode(nnkIdentDefs))

                elif c2.kind == nnkStmtList:
                    abs = false

                    c1[i2] = talkTree(
                        c2,
                        proc(node: NimNode, ctx: NimNode): NimNode =
                            result = copyNimNode(node)

                            if node.kind == nnkCall and node[0].kind == nnkDotExpr:
                                var
                                    current = node[0]

                                while current.kind in [nnkCall, nnkDotExpr]:
                                    current = current[0]

                                if current.kind == nnkIdent and current.strVal == "super":
                                    result = newNimNode(nnkCommand).add(
                                        newIdentNode("procCall"),
                                        talkTree(
                                            node,
                                            proc(node: NimNode, ctx: NimNode): NimNode =
                                                result = copyNimNode(node)

                                                if node.kind == nnkIdent:
                                                    if node.strVal == "super":
                                                        result = newNimNode(nnkCall).add(
                                                            newIdentNode(parent),
                                                            newIdentNode("this")
                                                        )
                                        )
                                    )

                            if node.kind == nnkIdent:
                                if node.strVal == "self":
                                    if stc == false:
                                        result = newIdentNode(target)

                                elif node.strVal == "proto":
                                    result = newIdentNode(parent)

                                elif node.strVal == "super":
                                    result = newNimNode(nnkCall).add(
                                        newIdentNode(parent),
                                        newIdentNode("this")
                                    )
                    )

            for i2, c2 in c1:
                if c2.kind == nnkFormalParams:
                    if stc == true:
                        c2[1].add(newIdentNode("self"))
                        c2[1].add(newNimNode(nnkBracketExpr).add(
                            newIdentNode("typedesc"),
                            newIdentNode(target)
                        ))

                    elif cpy == true:
                        c2[1].add(newIdentNode("this"))
                        c2[1].add(newIdentNode(target))

                    else:
                        c2[1].add(newIdentNode("this"))
                        c2[1].add(newNimNode(nnkVarTy).add(
                            newIdentNode(target)
                        ))

                    c2[1].add(newEmptyNode())

                    break;

            if abs:
                c1[c1.len - 1] = newNimNode(nnkStmtList).add(
                    quote do:
                        discard
                )

    body.insert(
        0,
        quote do:
            proc type*(this: `scope`): TypeID =
                result = `scope`.typeID
    )

    result = body



macro init*(self: typedesc, args: varargs[untyped]): untyped =
    if args.len > 0:
        result = quote do:
            block:
                var this = system.new(`self`)
                this.init(`args`)
                this
    else:
        result = quote do:
            block:
                var this = system.new(`self`)
                this.init()
                this
#[
    Responsible for resolving facetNodes and adding them to the config.
]#
macro resolve(facet: untyped): untyped =
    result = newStmtList()

    let
        currentPlan  = facetPlans[high facetPlans]
        currentClass = facet[0].strVal

    var
        callNode: NimNode = nil

    # Check if current facet has a call and remove corresponding node from property and store

    for i, node in facetNodes[currentPlan.index]:
        if node.kind == nnkExprColonExpr and node[0].strVal == "call":
            callNode = node[1]
            facet.del(i)

    for facetPlan in facetPlans:
        #[
            Ignore facet plans not relevant to the current class.

            For example, if we're currently trying to resolve a Delegate() we want to find facets
            on the shape of Delegate only.  We can then examine the facetPlan class to determine
            the type of facet and transform the current Delegate().  This effectively allows
            facets to be added to facets to change their behavior.
        ]#
        if currentPlan.class != facetPlan.scope:
            continue

        #[
            If current plan is shaped by a Hook, copy the call if necessary
        ]#
        if facetPlan.class == Hook and callNode == nil:
            for node in facetNodes[facetPlan.index]:
                if node.kind == nnkExprColonExpr and node[0].strVal == "call":
                    callNode = node[1]

                    facetNodes[currentPlan.index].insert(1, newColonExpr(
                        newIdentNode("call"),
                        copyNimTree(callNode)
                    ))

    # Add the call node back for non-Hook classes

    if callNode != nil and currentPlan.class != Hook:
        let
            this = newIdentNode("this")
            call = talkTree(
                callNode,
                proc(node: NimNode, ctx: NimNode): NimNode =
                    result = copyNimNode(node)

                    if node.kind == nnkIdent:
                        case node.strVal:
                            of "self":
                                result = newIdentNode(currentClass)
                            of "shape":
                                result = newIdentNode(currentPlan.realm)
            )

        facet.insert(1, newColonExpr(
            newIdentNode("call"),
            quote do:
                TrapHook.trap(
                    proc(`this`: Facet): TypedPointer =
                        result = Closure.trap(`call`)
                )
        ))

    result.add(
        quote do:
            config.add(`facet`)
    )

#[
    The shape macro is responsible for compile time aggegation of facet information which adds
    meta-information about each facet to facetPlans and a NimNode representation of the facet
    to facetNodes.  Once added, the node representations can be transformed and are added to
    the global config via resolve()
]#
macro shape*(scope: typedesc, body: untyped): untyped =
    result = newStmtList()

    var copy = copyNimTree(body)
    var count = 0

    #
    # Begin looping over each facet
    #

    for item in copy[0][1]:
        var facet: NimNode = nil
        var class: NimNode = nil

        if item.kind == nnkObjConstr:
            facet = item
        if item.kind == nnkCall:
            facet = newNimNode(nnkObjConstr).add(item[0]) # Convert to object

        if facet != nil:
            class = facet[0]

            inc count

            facet.insert(1, newColonExpr(
                newIdentNode("scope"),
                quote do: `scope`.TypeID
            ))

            facet.insert(1, newColonExpr(
                newIdentNode("class"),
                quote do: `class`.TypeID
            ))

            facetNodes.add(facet)

            var facetNodeIndex = newIntLitNode(facetNodes.len - 1)

            result.add(
                quote do:
                    static:
                        facetPlans.add((
                            realm: $`scope`,            # The class name for which the facet was added
                            scope: `scope`.TypeID,      # The class id for which the facet was added
                            class: `class`.TypeID,      # The class of the facet itself (e.g. Middleware)
                            index: `facetNodeIndex`     # The location of the facet's NimNode in facetNodes
                        ))

                    resolve(`facet`)
            )

    when defined(debug):
        echo "Loading shape for: ", scope.strVal, " (", count ," Facets)"

begin Facet:
    proc `[]`*(T: typedesc): T =
        result = T.value(this.call)
#        if defined this.call:

    proc matches*(class: typedesc, query: tuple = ()): bool =
        result = true

        if class.typeId notin [Facet.typeId, this.class]:
            result = false
        else:
            let target = % cast[class](this)
            let params = % query

            for key, val in params.pairs:
                if target.hasKey(key) and target[key] != val:
                    result = false
                    break

begin Config:
    method add*(facet: Facet): void {. base .}=
        this.data.add(facet)

    method len*(): int {. base .} =
        result = this.data.len()

    proc findAll*[T](class: typedesc[T], query: tuple = ()): seq[T] =
        for facet in this.data:
            if facet.matches(class, query):
                result.add(cast[T](facet))

    proc findOne*[T](class: typedesc[T], query: tuple = ()): T =
        let
            results = this.findAll(class, query)

        if results.len > 0:
            result = results[0]
        else:
            result = nil

begin App:
    method init*(config: Config): void {. base, mutator .}=
        this.config = config.deepCopy

        initLock(this.store.lock)

        for facet in this.config.data:
            facet.app = this

            if facet.call != nil:
                facet.call = TrapHook.value(facet.call)(facet)

        for hook in this.config.findAll(Hook, (init: true)):
            for facet in this.config.findAll(Facet, (class: hook.scope)):
                echo fmt "loading[{align($facet.class, 3, '0')}]: on facet scope {$facet.scope}"
                InitHook.value(facet.call)()