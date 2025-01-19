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

    Class* = ref object of RootObj

    Plan* = tuple[
        realm: string,
        class: TypeID,
        scope: TypeID,
        index: int
    ]

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

const
    facetNodes = CacheSeq("mininim.facets")
    nextTypeID = CacheCounter("mininim.types")

var
    facetPlans* {. compileTime .} = newSeq[Plan]()
    config* {. global .} = Config()

proc `%`*(p: pointer): JsonNode =
    result = JsonNode()

proc `%`*(t: tuple): JsonNode =
  result = newJObject()
  for k, v in t.fieldPairs():
    result[k] = %v

proc walkTree(node: NimNode, callback: proc(node: NimNode): NimNode): NimNode {. compileTime .} =
    result = callback(node)

    for child in node:
        result.add(walkTree(child, callback))

converter typeID*(T:typedesc): TypeID =
    const id = nextTypeID.value

    static:
        inc nextTypeID

    result = id.TypeID

macro begin*(scope: typedesc, body: untyped): untyped =
    result = quote do:
        `body`

macro init*(self: typedesc, args: varargs[untyped]): auto =
    if args.len > 0:
        result = quote do:
            var this: `self` = system.new(`self`)
            this.init(`args`)
            this
    else:
        result = quote do:
            var this: `self` = system.new(`self`)
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
        hookNode = walkTree(
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
    proc matches*(this: Facet, class: typedesc, query: tuple = ()): bool =
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
    method init*(this: var Config): void =
        this.data = newSeq[Facet]()

    method add*(this: var Config, facet: Facet): void =
        this.data.add(facet)

    method len*(this: var Config): int =
        result = this.data.len()

    proc findAll*[T](this: var Config, class: typedesc[T], query: tuple = ()): seq[T] =
        for facet in this.data:
            if facet.matches(class, query):
                result.add(facet.T)

    proc findOne*[T](this: var Config, class: typedesc[T], query: tuple = ()): T =
        let results = this.findAll(class, query)

        if results.len > 0:
            result = results[0]
        else:
            result = nil

begin App:
    method init*(this: var App, config: var Config): void {.base.} =
        this.config = config