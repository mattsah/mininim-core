import
    std/json,
    std/sugar,
    std/macros,
    std/tables,
    std/marshal,
    std/sequtils,
    std/macrocache,
    std/typetraits

export
    json,
    sugar,
    macros,
    tables,
    marshal,
    sequtils,
    typetraits

type
    TypeID* = uint16

    Class* {. inheritable .} = ref object

    App* = ref object of Class
        config*: Config

    Config* = ref object of Class
        data: seq[Facet]

    Facet* = ref object of Class
        hook*: pointer
        class*: TypeID
        scope*: TypeID

    Hook* = ref object of Facet
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
    config* {. global .} = Config()

proc init*(this: auto) =
    discard

proc `%`*(p: pointer): JsonNode =
    result = JsonNode()

proc `%`*(t: tuple): JsonNode =
  result = newJObject()
  for k, v in t.fieldPairs():
    result[k] = %v

proc talkTree(node: NimNode, callback: proc(node: NimNode): NimNode): NimNode {. compileTime .} =
    result = callback(node)

    for child in node:
        result.add(talkTree(child, callback))

converter typeID*(T:typedesc): TypeID =
    const id = nextTypeID.value

    static:
        inc nextTypeID

    result = id.TypeID

template static*() {. pragma .}

macro begin*(scope: typedesc, body: untyped) =
    let
        target = scope.strVal

    for i1, c1 in body:
        if c1.kind in [nnkMethodDef, nnkProcDef]:
            var stc = false
            var abs = true

            for i2, c2 in c1:
                if c2.kind == nnkPragma:
                    for i3, c3 in c2:
                        if c3.kind == nnkIdent:
                            if c3.strVal == "static":
                                stc = true

                elif c2.kind == nnkFormalParams:
                    if c2[0].kind == nnkIdent:
                        if c2[0].strVal == "self":
                            c2[0] = newIdentNode(target)

                    c2.insert(1, newNimNode(nnkIdentDefs))

                elif c2.kind == nnkStmtList:
                    abs = false

                    if stc == false:
                        c1[i2] = talkTree(
                            c2,
                            proc(node: NimNode): NimNode =
                                result = copyNimNode(node)

                                if node.kind == nnkIdent:
                                    if node.strVal == "self":
                                        result = newIdentNode(target)
                        )

            for i2, c2 in c1:
                if c2.kind == nnkFormalParams:
                    if stc == true:
                        c2[1].add(newIdentNode("self"))
                        c2[1].add(newNimNode(nnkBracketExpr).add(
                            newIdentNode("typedesc"),
                            newIdentNode(target)
                        ))
                        c2[1].add(newEmptyNode())

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

    var
        hookNode: NimNode
        proxyRealm: string

    let currentPlan = facetPlans[high facetPlans]

    block findCall:
        for facetPlan in facetPlans:
            if facetPlan.class == Hook.TypeID and currentPlan.class == facetPlan.scope:
                proxyRealm = facetPlan.realm

                for node in facetNodes[facetPlan.index]:
                    if node.kind == nnkExprColonExpr and node[0].strVal == "call":
                        hookNode = node[1]
                        break findCall

    if hookNode != nil:
        hookNode = talkTree(
            hookNode,
            proc(node: NimNode): NimNode =
                if node.kind == nnkIdent and node.strVal == proxyRealm:
                    result = newIdentNode(currentPlan.realm)
                else:
                    result = copyNimNode(node)
        )

        result.add(
            quote do:
                if `property`.hook == nil:
                    `property`.hook = `hookNode`
        )

    if currentPlan.class != Hook.TypeID:
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

        if this.class != class.typeID:
            result = false
        else:
            let target = % cast[class](this)
            let params = % query

            for key, val in params.getFields():
                if target.hasKey(key) and target[key] != val:
                    result = false
                    break

begin Config:
    method init*(): void {. base .} =
        this.data = newSeq[Facet]()

    method add*(facet: Facet): void {. base .}=
        this.data.add(facet)

    method len*(): int {. base .} =
        result = this.data.len()

    proc findAll*[T](class: typedesc[T], query: tuple = ()): seq[T] =
        for facet in this.data.mitems:
            if facet.matches(class, query):
                result.add(facet.T)

    proc findOne*[T](class: typedesc[T], query: tuple = ()): T =
        let results = this.findAll(class, query)

        if results.len > 0:
            result = results[0]
        else:
            result = nil

begin App:
    method init*(config: var Config): void {. base .}=
        this.config = config