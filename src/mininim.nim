import
    std/json,
    std/sugar,
    std/macros,
    std/tables,
    std/marshal,
    std/macrocache,
    std/typetraits,
    classes/internal,
    classes/plugin_variables,
    classes/plugin_methods,
    classes/plugin_destructors,
    classes/plugin_super,
    classes/plugin_mixins

export
    json,
    sugar,
    macros,
    tables,
    marshal,
    internal,
    typetraits

#
# Base Converters, Methods and Macros
#

const nextTypeID = CacheCounter("mininim.types")

type
    TypeID* = uint16

converter typeID*(T:typedesc): TypeID =
    const id = nextTypeID.value
    static:
        inc nextTypeID
    return id.TypeID

macro class*(head: untyped): untyped =
    return newClassDescription("class", head, newStmtList()).compile()

macro class*(head: untyped, body: untyped): untyped =
    return newClassDescription("class", head, body).compile()

macro init*(self: typedesc, args: varargs[untyped]): auto =
    if args.len > 0:
        result = quote do:
            let this: `self` = system.new(`self`)
            this.init(`args`)
            this
    else:
        result = quote do:
            let this: `self` = system.new(`self`)
            this

proc `%`*(p: pointer): JsonNode =
    result = JsonNode()

proc `%`*(t: tuple): JsonNode =
  result = newJObject()
  for k, v in t.fieldPairs():
    result[k] = %v

#
#   Core runtime library
#

class Facet:
    var
        hook*: pointer
        class*: TypeID
        scope*: TypeID

proc call*[T](this: Facet, calltype: typedesc[T]): T =
    result = cast[calltype](this.hook)

proc matches*(this: Facet, class: typedesc, query: tuple = ()): bool =
    if this.class != class.typeID:
        return false

    let target = % cast[class](this)
    let params = % query

    for key, val in params.getFields():
        if target.hasKey(key) and target[key] != val:
            return false

    return true

class Hook of Facet:
    var
        call*: pointer

class Config:
    var
        data: seq[Facet]

    method add*(facet: Facet): void {. base .} =
        this.data.add(facet)

    method len*(): int {. base .} =
        result = this.data.len

proc findOne*[T](this: Config, class: typedesc[T], query: tuple = ()): T =
    let results = this.findAll(class, query)

    if results.len > 0:
        result = results[0]
    else:
        result = nil

proc findAll*[T](this: Config, class: typedesc[T], query: tuple = ()): seq[T] =
    for facet in this.data:
        if facet.matches(class, query):
            result.add(facet.T)

var config* {. global .} = Config()

class App:
    var
        config*: Config

    method init*(config: var Config): void {.base.} =
        this.config = config

#
# Metaprogramming framework
#

type
    PlansSeq = seq[
        tuple[
            realm: string,
            class: TypeID,
            scope: TypeID,
            index: int
        ]
    ]

var facetNodes {. compileTime .}: CacheSeq = CacheSeq("mininim.facets")
var facetPlans {. compileTime .}: PlansSeq

proc walkTree(node: NimNode, callback: proc(node: NimNode): NimNode): NimNode {. compileTime .} =
    result = callback(node)

    for child in node:
        result.add(walkTree(child, callback))

macro resolve(property: untyped) =
    result = newStmtList()

    var
        hookNode: NimNode
        proxyRealm: string

    let currentPlan = facetPlans[high facetPlans]

    block findCall:
        for facetPlan in facetPlans:
            if facetPLan.class == Hook.TypeID and currentPlan.class == facetPlan.scope:
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
                    return newIdentNode(currentPlan.realm)

                return copyNimNode(node)
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