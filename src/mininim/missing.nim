import
    std/macrocache,
    std/parseutils,
    std/strformat,
    std/strutils

export
    macrocache,
    parseutils,
    strformat,
    strutils

#[
    Stored "type" support
]#
type
    TypeID* = uint16

const
    nextTypeID = CacheCounter("mininim.typeCounter")

converter typeID*(T: typedesc): TypeID =
    const id = nextTypeID.value

    static:
        when defined(debug):
            echo fmt "Registering {$T} with id {id}"

        inc nextTypeID

    result = id.TypeID

proc type*(self: typedesc): TypeID =
    result = self.typeID


#[
    Boolean converters
]#
converter toBool*(this: string): bool =
    if this.len == 0:
        result = false
    else:
        result = parseBool(this)
    when defined debug:
        echo fmt "Converted [string] {this} to bool {$result}"

converter toBool*(this: int): bool =
    result = this != 0
    when defined debug:
        echo fmt "Converted [int] {$this} to bool {$result}"

#[
    Accessors
]#

proc `[]`*[T](this: Slice[T], idx: int): T =
    for key, val in this:
        if key == idx:
            result = val
            break

#[
    Traversable and functional handling
]#

type
    iteratable*[T] = concept self
        T
        self.len is int
        for val in self:
                val is T

    traversable*[K, T] = concept self
        K
        T
        self.len is int
        for key, val in self:
            key is K
            val is T

iterator pairs*[T](this: iteratable[T]): (int, T) =
    var
        idx = 0

    for val in this:
        yield (idx, val)
        inc idx

proc map*[K, T, S](this: traversable[K, T], op: proc(x: T): S {. closure .}): seq[S] =
    var
        idx = 0

    newSeq(result, this.len)

    for val in this:
        result[idx] = op(val)
        inc idx


proc map*[K, T, S](this: traversable[K, T], op: proc(key: K, val: T): S {. closure .}): seq[S] =
    var
        idx = 0

    newSeq(result, this.len)

    for key, val in this:
        result[idx] = op(key, val)
        inc idx

proc map*[K, T, K2, T2](this: traversable[K, T], op: proc(key: K, val: T): (K2, T2) {. closure .}): seq[(K2, T2)] =
    var
        idx = 0

    newSeq(result, this.len)

    for key, val in this:
        result[idx] = op(key, val)
        inc idx
