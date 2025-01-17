import
    std/sugar,
    std/macros,
    std/macrocache,
    std/typetraits,
    classes/internal,
    classes/plugin_variables,
    classes/plugin_methods,
    classes/plugin_destructors,
    classes/plugin_super,
    classes/plugin_mixins

export
    sugar,
    macros,
    internal,
    typetraits

#
# Base Converters, Methods and Macros
#

const nextTypeID = CacheCounter("mininim.types")

type TypeID* = uint16

converter typeID*(T:typedesc): TypeID =
    const id = nextTypeID.value
    static:
        inc nextTypeID
    return id.TypeID

method init*(this: RootObj): void {.base.} =
    discard

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

#
#   Core library
#

class Facet:
    var
        id*: int
        kind*: TypeID
        target*: TypeID
        hook*: pointer


proc call*[T](this: Facet, calltype: typedesc[T]): T =
    result = cast[calltype](this.hook)

proc matches*(this: Facet, kind: typedesc): bool =
    return this.kind == kind.TypeID

proc matches*(this: Facet, target: typedesc, kind: typedesc): bool =
    return this.target == target.TypeID and this.kind == kind.TypeID

class Hook of Facet:
    var
        id*: 0
        call*: pointer

class Config:
    var
        data: seq[Facet]

    method add*(facet: Facet): void {. base .} =
        this.data.add(facet)

    method len*(): int {. base .} =
        result = this.data.len

proc findOne*[T](this: Config, kind: typedesc[T], target: typedesc): T =
    let results = this.findAll(kind, (target: target.TypeID))

    if results.len > 0:
        result = results[0]
    else:
        result = nil

proc findAll*[T](this: Config, kind: typedesc[T], query: tuple = ()): seq[T] =
    for facet in this.data:
        if facet.matches(kind):
            #
            # Implement query
            #
            result.add(facet.T)

var config* {. global .} = Config()

class App:
    var
        config*: Config

    method init*(config: var Config): void {.base.} =
        this.config = config

const facets = CacheSeq("mininim.facets")
var lookup {. compileTime .} : seq[
    tuple[
        domain: string,
        target: TypeID,
        kind: TypeID,
        idx: int
    ]
]

proc walkTree(node: NimNode, callback: proc(node: NimNode): NimNode): NimNode {. compileTime .} =
    result = callback(node)

    for child in node:
        result.add(walkTree(child, callback))

macro hook(property: untyped) =
    result = newStmtList()

    var
        hookNode: NimNode
        domain: string

    let current = lookup[high lookup]

    block findCall:
        for facet in lookup:
            if facet.kind == Hook.TypeID and current.kind == facet.target:
                domain = facet.domain
                for node in facets[facet.idx]:
                    if node.kind == nnkExprColonExpr and node[0].strVal == "call":
                        hookNode = node[1]
                        break findCall

    if hookNode != nil:
        hookNode = walkTree(
            hookNode,
            proc(node: NimNode): NimNode =
                if node.kind == nnkIdent and node.strVal == domain:
                    return newIdentNode(current.domain)

                return copyNimNode(node)
        )

        result = quote do:
            `property` = `hookNode`


macro shape*(target: typedesc, body: untyped) =
    result = newStmtList()

    var copy = copyNimTree(body)
    var count = 0

    for item in copy[0][1]:
        var node: NimNode = nil
        var kind: NimNode = nil

        if item.kind == nnkObjConstr:
            node = item
        if item.kind == nnkCall:
            node = newNimNode(nnkObjConstr).add(item[0]);

        if node != nil:
            kind   = node[0]
            count += 1

            node.insert(1, newColonExpr(
                newIdentNode("target"),
                quote do: `target`.TypeID
            ))

            node.insert(1, newColonExpr(
                newIdentNode("kind"),
                quote do: `kind`.TypeID
            ))

            facets.add(node)

            var facet = newIntLitNode(facets.len - 1)

            result.add(
                quote do:
                    static:
                        lookup.add((
                            domain: $`target`,
                            target: `target`.TypeID,
                            kind: `kind`.TypeID,
                            idx: `facet`
                        ))

                    var facet = `node`

                    hook(facet.hook)

                    config.add(facet)
            )

    when defined(debug):
        echo "Loading shape for: ", target.strVal, " (", count ," Facets)"