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
    macrocache,
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

    TrappedPointer* = ref object
        data: pointer
        name: string
        size: int

    InitHook = proc(): void {. closure, gcsafe .}
    TrapHook = proc(this: Facet): TrappedPointer {. nimcall .}
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
        call*: TrappedPointer

    Hook* = ref object of Facet
        init*: bool = false

    Shared* = ref object of Facet

    Plan* = tuple[
        realm: string,
        scope: TypeID,
        class: TypeID,
        index: int
    ]

const
    nextTypeID = CacheCounter("mininim.typeCounter")
    typeIndex = CacheTable("mininim.typeIndex")
    typeCode = CacheTable("mininim.typeCode")

var
    facetPlans {. compileTime .} = newSeq[Plan]()
    facetNodes {. compileTime .} = newSeq[NimNode]()

let
    config* = Config()

#[
    Converters
]#
converter typeID*(T: typedesc): TypeID =
    const id = nextTypeID.value

    static:
        when defined(debug):
            echo fmt "Registering {$T} with id {id}"

        inc nextTypeID

    result = id.TypeID

converter toBool*(this: string): bool =
    result = this.len > 0
    when defined debug:
        echo fmt "Converted [string] {this} to bool {$result}"

converter toBool*(this: int): bool =
    result = this != 0
    when defined debug:
        echo fmt "Converted [int] {$this} to bool {$result}"

proc `%`*(a: App): JsonNode =
    result = newJInt(cast[int](addr a))

proc `%`*(p: pointer): JsonNode =
    result = newJInt(cast[int](p))

proc `%`*(t: tuple): JsonNode =
    result = newJObject()

    for k, v in t.fieldPairs():
        result[k] = % v

proc type*(self: typedesc): TypeID =
    result = self.typeID

#[
    Constructor
]#

proc init*(this: auto) =
    discard

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

##
##    Class definitions
##



#[
    Get the typed/casted pointer back as for a given TrappedPointer
]#
proc get*(self: typedesc, this: TrappedPointer): ptr self {. gcsafe .} =
#    if $T != this.name:
#        raise newException(CatchableError, "Type mismatch: expected one of " & this.name & ", got " & $T)
#    if this.kind != Closure and sizeof(T) != this.size:
#        raise newException(CatchableError, "Size mismatch: expected " & $this.size & ", got " & $sizeof(T))

    # Handle cloosure

    result = cast[ptr self](this.data)

#[
    Get the typed/casted dereferenced pointer back for a given TrappedPointer
]#
proc value*(self: typedesc, this: TrappedPointer): self {. gcsafe .}=
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

#[
    Trap a variable's raw pointer (including closures) and get a TrappedPointer in its place
]#
proc trap*[V](self: typedesc, value: V): TrappedPointer =
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

    result = TrappedPointer(
        data: head,
        name: name,
        size: size,
    )

    when defined(debug):
        echo fmt "trapped[{align($result.size, 3, '0')}]: {result.name} @ {$cast[int](result.data)}"

##
##    Class definitions
##

template infix*() {. pragma .}
template static*() {. pragma .}
template mutator*() {. pragma .}
template abstract*() {. pragma .}

#[
    Recurisvely walk an entire NimNode's treee structure and recreate it.  The resulting TreeCall
    can either copy the node (with modificaitons) or create a totally new one in its place.  It
    can use the context (ctx) to look to the parent node and its siblings as well.
]#
proc talkTree(node: NimNode, call: TreeCall, ctx: NimNode = nil): NimNode {. compileTime .} =
    result = call(node, ctx)

    if result.len == 0 and result.kind == node.kind:
        for i, child in node:
            result.add(talkTree(child, call, node))

#[
    Transforms all proc, method, template, converters, and iterators with requisite OOP programming
    paradigms.  This adds suppor for implied `this`, `self`, `super`, and `proc`, taking hints
    from the pragma.
]#
macro begin*(scope: typedesc, body: untyped) =
    var
        parent: string

    let
        target = scope.strVal
        original = copyNimTree(body)

    if scope.getImpl[2][0].len > 0 and scope.getImpl[2][0][1].len > 0:
        parent = scope.getImpl[2][0][1][0].strVal
    else:
        parent = "Class"

    #
    # Replace all static keyword instances
    #

    result = body.talkTree(
        proc(node: NimNode, ctx: NimNode): NimNode =
            result = copyNimNode(node)

            if node.kind == nnkIdent:
                if node.strVal == "self":
                    result = newIdentNode(target)
                elif node.strVal == "proto":
                    result = newIdentNode(parent)
    )

    #
    # Do parameter insertion and proc/method/template/etc call replacements
    #

    for i1, c1 in result:
        if c1.kind in [nnkMethodDef, nnkProcDef, nnkMacroDef, nnkTemplateDef, nnkConverterDef, nnkIteratorDef]:
            var
                stc = false
                abs = false
                ifx = false
                cpy = true
                loc = 1

            if c1[4].kind == nnkPragma:
                for i2, c2 in c1[4]:
                    if c2.kind == nnkIdent:
                        if c2.strVal == "static":
                            stc = true
                        if c2.strVal == "mutator":
                            cpy = false
                        if c2.strVal == "abstract":
                            abs = true
                        if c2.strVal == "infix":
                            ifx = true

            for i2, c2 in c1:
                if c2.kind == nnkFormalParams:
                    if ifx:
                        loc = c2.len

                    c2.insert(loc, newNimNode(nnkIdentDefs)) # insert first parameter placeholder

                    if stc == true:
                        c2[loc].add(newIdentNode("self"))
                        c2[loc].add(newNimNode(nnkBracketExpr).add(
                            newIdentNode("typedesc"),
                            newIdentNode(target)
                        ))

                    elif cpy == true:
                        c2[loc].add(newIdentNode("this"))
                        c2[loc].add(newIdentNode(target))

                    else:
                        c2[loc].add(newIdentNode("this"))
                        c2[loc].add(newNimNode(nnkVarTy).add(
                            newIdentNode(target)
                        ))

                    c2[loc].add(newEmptyNode())

                if c2.kind == nnkStmtList:
                    var
                        transform: TreeCall

                    transform = proc(node: NimNode, ctx: NimNode): NimNode =
                            result = copyNimNode(node)

                            if node.kind == nnkCall and node[0].kind == nnkDotExpr:
                                var
                                    current = node[0]

                                while current.kind in [nnkCall, nnkDotExpr]:
                                    current = current[0]

                                if current.kind == nnkIdent:
                                    if current.strVal == "super":
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
                                if node.strVal == "super" and not stc:
                                    result = newNimNode(nnkCall).add(
                                        newIdentNode(parent),
                                        newIdentNode("this")
                                    )

                                elif node.strVal == "fmt":
                                    var
                                        idx = 0
                                        cnt = 0
                                        str = ""
                                        tmp = ""
                                        ast: NimNode

                                    while idx < ctx[1].strVal.len:
                                        if ctx[1].strVal[idx] == '}':
                                            if cnt > 0:
                                                dec cnt

                                            if cnt == 0:
                                                ast = parseStmt(tmp)
                                                tmp = ""

                                                str.add(
                                                    repr(talkTree(ast, transform))
                                                        .strip(chars = {' ', '\t', '\n'})
                                                )

                                        if cnt > 0:
                                            tmp.add(ctx[1].strVal[idx])
                                        else:
                                            str.add(ctx[1].strVal[idx])

                                        if ctx[1].strVal[idx] == '{':
                                            inc cnt

                                        inc idx

                                    ctx[1] = newStrLitNode(str)

                    c1[i2] = talkTree(c2, transform)

            if abs:
                c1[c1.len - 1] = newNimNode(nnkStmtList).add(
                    quote do:
                        discard
                )

    result.insert(
        0,
        quote do:
            when $(`scope`.type) notin typeIndex:
                static:
                    typeIndex[$(`scope`.type)] = quote do:
                        ()

                proc type*(this: `scope`): TypeID =
                    result = `scope`.typeID
    )

    result.insert(
        0,
        quote do:
            when (currentSourcePath() & '.' & `target`) notin typeCode:
                static:
                    # TODO: Actually figure out how to cache this
                    typeCode[currentSourcePath() & '.' & `target`] = newStmtList()
    )

##
##    Class definitions
##

#[
    The resolve macro is responsible for finalizing the facet object and adding it to the
    global configuration.  This includes setting up call properties based on hooks as well
    as any transformation of OOP keywords.
]#
macro resolve(facet: untyped): untyped =
    result = newStmtList()

    let
        target = facet[0].strVal
        currentPlan = facetPlans[high facetPlans]

    var
        callNode: NimNode

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
                                result = newIdentNode(target)
                            of "shape":
                                result = newIdentNode(currentPlan.realm)
            )

        facet.insert(1, newColonExpr(
            newIdentNode("call"),
            quote do:
                TrapHook.trap(
                    proc(`this`: Facet): TrappedPointer =
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

    var
        copy = copyNimTree(body)
        count = 0

    #
    # Begin looping over each facet
    #

    for item in copy[0][1]:
        var
            facet: NimNode = nil
            class: NimNode = nil

        if item.kind == nnkObjConstr:
            facet = item
        if item.kind == nnkCall:
            facet = newNimNode(nnkObjConstr).add(item[0]) # Convert to object

        if facet != nil:
            class = facet[0]

            let
                target = scope.strVal
                entry = copyNimTree(body)

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

##
##    Class definitions
##

#[

]#
begin Facet:
    proc `[]`*(T: typedesc): T =
        result = T.value(this.call)

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

#[

]#
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

begin Shared:
    discard

shape Shared: @[
    Hook(
        init: true,
        call: proc(): void {. closure .} =
            discard this.app.get(shape)
    )
]

#[

]#
begin App:
    method init*(): void {. base, mutator .}=
        this.config = deepCopy config

        initLock(this.store.lock)

        for facet in this.config.data:
            facet.app = this

            if facet.call != nil:
                facet.call = TrapHook.value(facet.call)(facet)

        for hook in this.config.findAll(Hook, (init: true)):
            for facet in this.config.findAll(Facet, (class: hook.scope)):
                echo fmt "loading[{align($facet.class, 3, '0')}]: on facet scope {$facet.scope}"
                InitHook.value(facet.call)()