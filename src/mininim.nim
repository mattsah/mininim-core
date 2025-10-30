import
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
    std/locks


export
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
    locks

type
    InitHook* = proc(app: App): void {. nimcall .}
    TreeCall = proc(node: NimNode, ctx: NimNode): NimNode

    TypeID* = uint16

    Class* {. inheritable .} = ref object

    Storage* = object
        instances*: Table[TypeId, ref RootObj]
        lock*: Lock

    App* = ref object of Class
        config*: Config
        store*: Storage

    Config* = ref object of Class
        data*: seq[Facet]

    Facet* = ref object of Class
        hook*: pointer
        class*: TypeID
        scope*: TypeID

    Hook* = ref object of Facet
        init*: bool = false
        swap*: TypeID
        call*: pointer

    Plan* = tuple[
        realm: string,
        class: TypeID,
        scope: TypeID,
        index: int
    ]

const
    facetNodes = CacheSeq("mininim.facets")
    nextTypeID = CacheCounter("mininim.types")

var
    facetPlans* {. compileTime .} = newSeq[Plan]()

let
    config* = Config()

proc init*(this: auto) =
    discard

proc `%`*(p: pointer): JsonNode =
    result = JsonNode()

proc `%`*(t: tuple): JsonNode =
  result = newJObject()
  for k, v in t.fieldPairs():
    result[k] = % v

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

    if scope.getImpl[2][0].len > 0:
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

macro resolve(property: untyped) =
    result = newStmtList()

    let currentPlan = facetPlans[high facetPlans]

    for facetPlan in facetPlans:
        if currentPlan.class != facetPlan.scope:
            continue

        #[
            Handle hooks
        ]#
        if facetPlan.class == Hook.TypeID:
            var
                hookNode: NimNode = nil
                swapIdent: string

            # Look to see if we have a pre-defined hook
            for node in facetNodes[currentPlan.index]:
                if node.kind == nnkExprColonExpr and node[0].strVal == "hook":
                    hookNode = node[1]

            # If no pre-defined hook, use the facet plan as a template.
            if hookNode == nil:
                swapIdent = facetPlan.realm

                for node in facetNodes[facetPlan.index]:
                    if node.kind == nnkExprColonExpr and node[0].strVal == "call":
                        hookNode = node[1]
                    elif node.kind == nnkExprColonExpr and node[0].strVal == "swap":
                        swapIdent = node[1].strVal

                if hookNode != nil:
                    hookNode = talkTree(
                        hookNode,
                        proc(node: NimNode, ctx: NimNode): NimNode =
                            if node.kind == nnkIdent and node.strVal == swapIdent:
                                result = newIdentNode(currentPlan.realm)
                            else:
                                result = copyNimNode(node)
                    )

                    result.add(
                        quote do:
                            `property`.hook = `hookNode`
                    )

    result.add(
        quote do:
            config.add(`property`)
    )

macro shape*(scope: typedesc, body: untyped) =
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
                            realm: $`scope`,
                            class: `class`.TypeID,
                            scope: `scope`.TypeID,
                            index: `facetNodeIndex`
                        ))

                    var facet = `facet`

                    resolve(facet)
            )

    when defined(debug):
        echo "Loading shape for: ", scope.strVal, " (", count ," Facets)"

begin Facet:
    proc matches*(class: typedesc, query: tuple = ()): bool =
        result = true

        if class.typeId != Facet.typeId and this.class != class.typeID:
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
        this.config = config

        initLock(this.store.lock)

        for hook in this.config.findAll(Hook, (init: true)):
            for facet in this.config.findAll(Facet, (class: hook.scope)):
                cast[InitHook](facet.hook)(this)
