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
    internal,
    typetraits

#
# Coverters, Methods and Macros
#

type TypeID* = uint16

const nextTypeID = CacheCounter("nextTypeID")

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

macro init*(self: typedesc, args: varargs[var typed, typed]): auto =
    result = quote do:
        let this: `self` = system.new(`self`)
        this.init(`args`)
        this

macro shape*(target: typedesc, body: typed) =
    result = newStmtList()

    var copy = copyNimTree(body)

    when defined(debug):
        echo "Loading shapes for: ", target.strVal

    for item in copy[0][1]:
        var node: NimNode = nil

        if item.kind == nnkObjConstr:
            node = item
        elif item.kind == nnkHiddenSubConv:
            node = item[1]

        if node != nil:
            node.insert(1, newColonExpr(
                newIdentNode("kind"),
                newDotExpr(
                    newIdentNode(node[0].strVal),
                    newIdentNode("TypeID")
                )
            ))

            result = quote do:
                config.add((target: `target`.TypeID, facet: `node`.Facet))

#
#   Core library
#

class Facet:
    var kind*: TypeID

type
    Config* = seq[tuple[target: TypeID, facet: Facet]]

    App* = ref object of RootObj
        config*: Config

method init*(this: App, config: var Config): void {.base.} =
    this.config = config

proc matches*(this: Facet, kind: typedesc): bool =
    return this.kind == kind.TypeID

var config*: Config