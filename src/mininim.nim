import
    std/sugar,
    std/dynlib,
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
    dynlib,
    internal,
    typetraits

#
# Base Coverters, Methods and Macros
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
        kind*: TypeID
        target*: TypeID
        hook*: pointer

class Hook of Facet:
    var
        call*: string

class Config:
    var
        data: seq[Facet]

    method add*(facet: Facet): void {. base .} =
        this.data.add(facet)

    method len*(): int {. base .} =
        result = this.data.len

class App:
    var
        config*: Config

method init*(this: App, config: var Config): void {.base.} =
    this.config = config

proc matches*(this: Facet, kind: typedesc): bool =
    return this.kind == kind.TypeID

proc matches*(this: Facet, target: typedesc, kind: typedesc): bool =
    return this.target == target.TypeID and this.kind == kind.TypeID

proc findOne*(this: var Config, target: typedesc, kind: typedesc): Facet =
    let results = this.findAll(kind, (kind: target.TypeID))

    if results.len > 0:
        result = results[0]
    else:
        result = nil

proc findAll*(this: var Config, kind: typedesc, query: tuple = ()): seq[Facet] =
    result = @[]

    for facet in this.data:
        if facet.matches(kind):
            result.add(facet)

var config* {. global .}: Config = Config()
let lib*: LibHandle = loadLib()


macro shape*(target: typedesc, body: typed) =
    result = newStmtList()

    var copy  = copyNimTree(body)
    var count = 0

    for item in copy[0][1]:
        var node: NimNode = nil
        var kind: NimNode = nil

        if item.kind == nnkObjConstr:
            node = item
        elif item.kind == nnkHiddenSubConv:
            node = item[1]

        if node != nil:
            kind = node[0]

            node.insert(1, newColonExpr(
                newIdentNode("hook"),
                quote do: (
                    let facet = config.findOne(`target`, Hook)

                    if facet == nil:
                        nil.pointer
                    else:
                        lib.symAddr(facet.Hook.call)
                )
            ))

            node.insert(1, newColonExpr(
                newIdentNode("target"),
                quote do: `target`.TypeID
            ))

            node.insert(1, newColonExpr(
                newIdentNode("kind"),
                quote do: `kind`.TypeID
            ))

            count += 1

            result.add(
                quote do:
                    config.add(`node`.Facet)
            )

    when defined(debug):
        echo "Loading shape for: ", target.strVal, " (", count ," Facets)"
