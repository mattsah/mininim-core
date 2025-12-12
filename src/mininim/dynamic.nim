{. experimental: "dotOperators" .}

import
    mininim

type
    dynType* = enum
        dynEmpty,
        dynInt,
        dynFloat,
        dynString,
        dynObject,
        dynArray,
        dynBool,
        dynNull

    dynToInt* = proc(this: dyn): int {. nimcall .}
    dynToFloat* = proc(this: dyn): float {. nimcall .}
    dynToString* = proc(this: dyn): string {. nimcall .}
    dynToBool* = proc(this: dyn): bool {. nimcall .}

    dyn* = ref object of Class
        kind: dynType = dynEmpty
        intVal: int
        floatVal: float
        stringVal: string
        objectVal: Table[string, dyn]
        arrayVal: seq[dyn]
        boolVal: bool
        toInt*: dynToInt
        toFloat*: dynToFloat
        toString*: dynToString
        toBool*: dynToBool

    FunctionRegistry* = Table[string, Function]
    FunctionWrapper* = proc(this: dyn, args: seq[dyn]): dyn
    Function* = ref object of Class
        minArgc*: int
        maxArgc*: int
        call*: FunctionWrapper
var
    allFunctions = FunctionRegistry()

template null*(): dyn =
    dyn(kind: dynNull)

#[
    Converters to dyn type
]#
converter asDyn*(this: typeof(nil)): dyn =
    when debugging(3):
        echo fmt "Converting [nil] to dynamic value"
    result = dyn(kind: dynNull)

converter asDyn*(this: dyn): dyn =
    if this == nil:
        result = asDyn(nil)
    else:
        result = this

converter asDyn*(this: int): dyn =
    when debugging(3):
        echo fmt "Converting [int] to dynamic value"
    result = dyn(kind: dynInt, intVal: this)

converter asDyn*(this: float): dyn =
    when debugging(3):
        echo fmt "Converting [float] to dynamic value"
    result = dyn(kind: dynFloat, floatVal: this)

converter asDyn*(this: string): dyn =
    when debugging(3):
        echo fmt "Converting [string] to dynamic value"
    result = dyn(kind: dynString, stringVal: this)

converter asDyn*(this: bool): dyn =
    when debugging(3):
        echo fmt "Converting [bool] to dynamic value"
    result = dyn(
        kind: dynBool,
        boolVal: this
    )

converter asDyn*[T](this: openArray[T]): dyn =
    when debugging(3):
        echo fmt "Converting [openArray[{$T}]] to dynamic value"
    result = dyn(
        kind: dynArray,
        arrayVal: newSeq[dyn]()
    )
    when T is dyn:
        result.arrayVal = this.toSeq()
    else:
        if this.len > 0:
            for val in this:
                result.arrayVal.add(asDyn(val))

converter asDyn*(this: object): dyn =
    when debugging(3):
        echo fmt "Converting [object] to dynamic value"
    result = dyn(
        kind: dynObject,
        objectVal: initTable[string, dyn](this.len)
    )
    for key, val in fieldPairs(this):
        result.objectVal[key] = asDyn(val)

converter asDyn*[K: auto, T: auto](this: Table[K, T]): dyn =
    when debugging(3):
        echo fmt "Converting [Table[{$K}, {$T}]] to dynamic value"
    result = dyn(
        kind: dynObject,
        objectVal: initTable[string, dyn](this.len)
    )
    for key, val in this.pairs:
        result.objectVal[$key] = asDyn(val)

converter asDyn*(this: tuple): dyn =
    when debugging(3):
        echo fmt "Converting [tuple] to dynamic value"
    var
        cur = 0
        table = false
        value = newSeq[(string, dyn)](this.len)
    if this.tupleLen == 0:
        table = true
    else:
        for key, val in this.fieldPairs:
            if key != "Field" & $cur:
                table = true
            value.add((key, asDyn(val)))
            inc cur
    if table:
        result = dyn(
            kind: dynObject,
            objectVal: value.toTable()
        )
    else:
        result = dyn(
            kind: dynArray,
            arrayVal: value.mapIt(it[1])
        )

#[
    Convert basically anything to a dynamic value
]#
macro `~`*(value: untyped): dyn =
    var
        arrayVal: NimNode
        realVal: NimNode

    if value.kind == nnkPar:
        realVal = value[0]
    else:
        realVal = value

    if realVal.kind == nnkBracket:
        arrayVal = realVal
    elif realVal.kind == nnkPrefix and realVal[0].strVal == "@" and realVal[1].kind == nnkBracket:
        arrayVal = realVal[1]

    if arrayVal != nil:
        if arrayVal.len == 0:
            result = quote do:
                dyn(kind: dynArray, arrayVal: newSeq[dyn]())
        else:
            let
                args = newNimNode(nnkArgList)
            for child in arrayVal:
                args.add(
                    quote do:
                        ~`child`
                )

            result = quote do:
                dyn(kind: dynArray, arrayVal: @[`args`])

    elif realVal.kind == nnkBracketExpr:
        let
            dyn = realVal[0]
            key = realVal[1]
        result = quote do:
            asDyn(`dyn`)[`key`]
    else:
        result = quote do:
            asDyn(`realVal`)

template `~@`*(value: untyped): untyped =
    ~ value

template `:=`*(this: untyped, value: untyped): untyped =
    var this: dyn = ~(value)


#[
    Primary dyn implementation
]#

begin dyn:
    #
    # Forward declare for dollar (avoids ambiguous conversion)
    #

    proc `$`*(): string # Forward declaration for debug messages

    #
    # Basic type checking and conversion
    #

    #[

    ]#
    proc `as`*[T](to: typedesc[T]): self =
        result = null

        when T.type == bool:
            result = ~asBool(this)
        when T.type == int:
            result = ~asInt(this)
        when T.type == float:
            result = ~asFloat(this)
        when T.type == string:
            result = ~asString(this)
        when T.type == array:
            result = ~asArray(this)
        when T.type == object:
            result = ~asObject(this)

    proc `of`*(that: typeof(nil)): bool =
        if this == nil:
            result = true
        else:
            result = this.kind == dynNull

    proc `of`*[T](that: typedesc[T]): bool =
        result = false
        when T.type == bool:
            result = this.kind == dynBool
        when T.type == int:
            result = this.kind == dynInt
        when T.type == float:
            result = this.kind == dynFloat
        when T.type == string:
            result = this.kind == dynString
        when T.type == array:
            result = this.kind == dynArray
        when T.type == object:
            result = this.kind == dynObject
        when T.type == dyn:
            result = true

    proc `of`*(that: dynType): bool =
        result = this.kind == that

    proc `of`*(that: self): bool =
        result = this.kind == that.kind

    proc copy*(): dyn =
        result = dyn(kind: this.kind)
        case this.kind:
            of dynObject:
                result.objectVal = this.objectVal
            else:
                result = deepcopy this


    #
    # Converters to other types
    #

    converter asBool*(): bool =
        if this of nil:
            return false
        elif this.toBool != nil:
            result = this.toBool(this)
        elif this.kind == dynBool:
            return this.boolVal
        else:
            when debugging(3):
                echo fmt "Converting dynamic value [{this.kind}] to bool"
            case this.kind:
                of dynInt:
                    result = this.intVal > 0
                of dynFloat:
                    result = this.floatVal > 0
                of dynString:
                    if this.stringVal == "true":
                        result = true
                    elif this.stringVal == "false":
                        result = false
                    else:
                        result = this.stringVal.len > 0
                of dynArray:
                    result = this.arrayVal.len > 0
                of dynObject:
                    result = this.objectVal.len > 0
                else:
                    raise newException(ValueError, fmt "Cannot convert [{this.kind}] to bool")

    converter asInt*(): int =
        if this of nil:
            result = 0
        elif this.toInt != nil:
            result = this.toInt(this)
        elif this.kind == dynInt:
            result = this.intVal
        else:
            when debugging(3):
                echo fmt "Converting dynamic value [{this.kind}] to int"
            case this.kind:
                of dynBool:
                    result = if this.boolVal: 1 else: 0
                of dynFloat:
                    result = int(this.floatVal)
                of dynString:
                    var
                        floatVal: float
                        intVal: int
                    if this.stringVal.parseFloat(floatVal) == this.stringVal.len:
                        result = int(floatVal)
                    elif this.stringVal.parseInt(intVal) == this.stringVal.len:
                        result = intVal
                    else:
                        result = this.stringVal.len
                of dynArray:
                    result = this.arrayVal.len
                of dynObject:
                    result = this.objectVal.len
                else:
                    raise newException(ValueError, fmt "Cannot convert [{this.kind}] to int")

    converter asFloat*(): float =
        if this of nil:
            result = 0.0
        elif this.toFloat != nil:
            result = this.toFloat(this)
        elif this.kind == dynFloat:
            result = this.floatVal
        else:
            when debugging(3):
                echo fmt "Converting dynamic value [{this.kind}] to float"
            case this.kind:
                of dynBool:
                    result = if this.boolVal: 1.0 else: 0.0
                of dynInt:
                    result = float(this.intVal)
                of dynString:
                    var
                        floatVal: float
                        intVal: int
                    if this.stringVal.parseFloat(floatVal) == this.stringVal.len:
                        result = floatVal
                    elif this.stringVal.parseInt(intVal) == this.stringVal.len:
                        result = float(intVal)
                    else:
                        result = float(this.stringVal.len)
                of dynArray:
                    result = float(this.arrayVal.len)
                of dynObject:
                    result = float(this.objectVal.len)
                else:
                    raise newException(ValueError, fmt "Cannot convert [{this.kind}] to float")

    converter asString*(): string =
        if this of nil:
            result = "null"
        elif this.toString != nil:
            result = this.toString(this)
        elif this.kind == dynString:
            result = this.stringVal
        else:
            when debugging(3):
                echo fmt "Converting dynamic value [{this.kind}] to string"
            case this.kind:
                of dynBool:
                    result = if this.boolVal: "true" else: "false"
                of dynInt:
                    result = $this.intVal
                of dynFloat:
                    result = $this.floatVal
                of dynArray:
                    result = this.arrayVal.join()
                of dynObject:
                    result = $this.objectVal
                else:
                    raise newException(ValueError, fmt "Cannot convert [{this.kind}] to string")

    converter asArray*(): seq[self] =
        if this of nil:
            result = newSeq[self]()
        if this.kind == dynArray:
            result = this.arrayVal
        else:
            when debugging(3):
                echo fmt "Converting dynamic value [{this.kind}] to array (sequence)"
            case this.kind:
                of dynObject:
                    result = toSeq(this.objectVal.values)
                else:
                    result = @[this]

    converter asObject*(): Table[string, self] =
        if this of nil:
            result = initTable[string, dyn]()
        elif this.kind == dynObject:
            result = this.objectVal
        else:
            raise newException(ValueError, fmt "Cannot convert [{this.kind}] to object")

    #
    # Syntax and helpers
    #

    #
    # Misc common syntax, sugar, and accessors
    #

    #[
        Shorthand for string conversion
    ]#
    proc `$`*(): string =
        if this == nil: # Handled explicitly as you can't pass along null ref
            result = "null"
        else:
            result = asString(this)

    #[
        Shorthand for sequence conversion
    ]#
    proc `@`*(): seq[self] =
        if this == nil: # Handled explicitly as you can't pass along null ref
            result = newSeq[self]()
        else:
            result = asArray(this)

    #[
        Backwards index handler
    ]#

    proc `^`*(): int =
        case this.kind:
            of dynArray:
                result = this.arrayVal.len - 1
            of dyNString:
                result = this.stringVal.len - 1
            else:
                result = 0

    template `..^`*(start: untyped): untyped {. infix .} =
        start .. ^this

    #[
        Array Access
    ]#

    proc `[]`*(key: dyn): self =
        if key of nil:
            raise newException(ValueError, "Cannot use null key for array/object access")

        case this.kind:
            of dynObject:
                let
                    key = asString(key)

                if this.objectVal.hasKey(key):
                    result = this.objectVal[key]
                else:
                    echo getStackTrace()
                    raise newException(
                        ValueError,
                        fmt "Accessing '{key}' failed, value not defined"
                    )
            of dynArray:
                var
                    key = asInt(key)
                if key < 0:
                    key = this.arrayVal.len + key
                if key <= this.arrayVal.high and key >= this.arrayVal.low:
                    result = this.arrayVal[key]
                else:
                    raise newException(
                        ValueError,
                        fmt "Accessing '{key}' failed, outside range"
                    )
            of dynString:
                var
                    key = asInt(key)
                if key < 0:
                    key = this.stringVal.len + key
                if key <= this.stringVal.high and key >= this.stringVal.low:
                    result = $this.stringVal[key]
                else:
                    raise newException(
                        ValueError,
                        fmt "Accessing '{key}' failed, outside range"
                    )
            else:
                raise newException(
                    ValueError,
                    fmt "Cannot read key/field '{key}' from scalar dynamic value"
                )

    proc `[]=`*(key: dyn, value: auto): void =
        if key of nil:
            raise newException(ValueError, "Cannot use null key for array/object access")

        case this.kind:
            of dynObject:
                let
                    key = asString(key)

                when value is self:
                    if value of nil:
                        if this.objectVal.hasKey(key):
                            this.objectVal.del(key)
                    else:
                        this.objectVal[key] = value
                else:
                    this.objectVal[key] = value
            of dynArray:
                let
                    key = asInt(key)

                when value is self:
                    if value of nil:
                        this.arrayVal.delete(key)
                    else:
                        this.arrayVal[key] = value
                else:
                    this.arrayVal[key] = value
            else:
                raise newException(
                    ValueError,
                    fmt "Cannot assign property to non object/array data"
                )

    #[
        Dot notations
    ]#

    template `.`*(field: untyped): self =
        this[astToStr(field)]

    template `.=`*(field: untyped, value: auto): void =
        this[astToStr(field)] = value

    template `.()`*(call: untyped, args: varargs[self]): self =
        let
            key = astToStr(call)

        if self.hasFunction(key):
            self.callFunction(key, this, @args)
        else:
            this.call

    #
    # Equality and boolean operations
    #

    proc `not`*(): bool =
        if this of nil:
            result = true
        else:
            case this.kind:
                of dynBool: # optimize for bools directly
                    result = not this.boolVal
                else: # retain asBool centralized logic for all others
                    result = not asBool(this)

    #[
        Equal
    ]#

    proc `==`*(that: self): bool =
        var
            error = false

        if this of nil:
            result = that.kind == dynNull
        elif that of nil:
            result = this.kind == dynNull
        elif that of bool or this of bool:
            result = asBool(this) == asBool(that)
        elif (this of int or that of int) or (this of float or that of float):
            result = asFloat(this) == asFloat(that)
        else:
            case this.kind:
                of dynString:
                    case that.kind:
                        of dynString:
                            result = this.stringVal == asString(that)
                        else:
                            error = true
                of dynArray:
                    case that.kind:
                        of dynArray:
                            if this.arrayVal.len == that.arrayVal.len:
                                result = true # this.arrayVal == that.arrayVal
                        else:
                            error = true
                of dynObject:
                    case that.kind:
                        of dynObject:
                            if this.objectVal.len == that.objectVal.len:
                                result = true # this.objectVal == that.objectVal
                        else:
                            error = true
                else:
                    error = true
        if error:
            raise newException(ValueError, "Cannot compare (==) [{this.kind}] to [{that.kind}]")

    proc `==`*(that: auto): bool =
        result = this == asDyn(that)

    proc `==`*(that: auto): bool {. infix .}=
        result = this == asDyn(that)

    #[
        Not Equal
    ]#
    proc `!=`*(that: self): bool =
        if this of nil:
            result = that.kind != dynNull
        elif that of nil:
            result = this.kind != dynNull
        else:
            result = not(this == that)

    proc `!=`*(that: auto): bool =
        result = this != asDyn(that)

    proc `!=`*(that: auto): bool {. infix .}=
        result = this != asDyn(that)

    #[
        Greater Than
    ]#
    proc `>`*(that: self): bool =
        var
            error = false

        if this of nil:
            result = false
        elif that of nil:
            result = that.kind != dynNull
        elif that of bool or this of bool:
            result = (asBool(this) == true) and (asBool(that) == false)
        elif (this of int or that of int) or (this of float or that of float):
            result = asFloat(this) > asFloat(that)
        else:
            case this.kind:
                of dynString:
                    case that.kind:
                        of dynString:
                            result = this.stringVal > asString(that)
                        else:
                            error = true
                of dynArray:
                    case that.kind:
                        of dynArray:
                            if this.arrayVal.len > that.arrayVal.len:
                                result = true;
                            else:
                                error = true
                        else:
                            discard
                of dynObject:
                    case that.kind:
                        of dynObject:
                            if this.objectVal.len > that.objectVal.len:
                                result = true;
                            else:
                                error = true
                        else:
                            error = true
                else:
                    error = true
        if error:
            raise newException(ValueError, "Cannot compare (>) [{this.kind}] to [{that.kind}]")

    proc `>`*(that: auto): bool =
        result = this > asDyn(that)

    proc `>`*(that: auto): bool {. infix .}=
        result = this > asDyn(that)

    #[
        Greater Than or Equal
    ]#
    proc `>=`*(that: self): bool =
        result = (this > that) or (this == that)

    proc `>=`*(that: auto): bool =
        result = this >= asDyn(that)

    proc `>=`*(that: auto): bool {. infix .}=
        result = this >= asDyn(that)

    #[
        Less Than
    ]#
    proc `<`*(that: self): bool =
        var
            error = false

        if this of nil:
            result = that.kind != dynNull
        elif that of nil:
            result = true
        elif that of bool or this of bool:
            result = (asBool(this) == false) and (asBool(that) == true)
        elif (this of int or that of int) or (this of float or that of float):
            result = asFloat(this) < asFloat(that)
        else:
            case this.kind:
                of dynString:
                    case that.kind:
                        of dynString:
                            result = this.stringVal < asString(that)
                        else:
                            error = true
                of dynArray:
                    case that.kind:
                        of dynArray:
                            if this.arrayVal.len < that.arrayVal.len:
                                result = true;
                            else:
                                error = true
                        else:
                            error = true
                of dynObject:
                    case that.kind:
                        of dynObject:
                            if this.objectVal.len > that.objectVal.len:
                                result = true;
                            else:
                                error = true
                        else:
                            error = true
                else:
                    error = true
        if error:
            raise newException(ValueError, "Cannot compare (<) [{this.kind}] to [{that.kind}]")

    #[
        Less Than or Equal
    ]#
    proc `<=`*(that: self): bool =
        result = (this < that) or (this == that)

    proc `<=`*(that: auto): bool =
        result = this <= asDyn(that)

    proc `<=`*(that: auto): bool {. infix .}=
        result = this <= asDyn(that)

    #[

    ]#
    proc `and`*(that: auto): bool =
        if this of nil:
            result = asBool(null) and that
        else:
            result = asBool(this) and that # convert known dyn value to force second if needed

    proc `or`*(that: auto): bool =
        if this of nil:
            result = asBool(null) or that
        else:
            result = asBool(this) or that # convert known dyn value to force second if needed

    #
    # Mutation operators (needs to come before other operators, as they may be used by them)
    #

    proc `<<`*(): self =
        case this.kind:
            of dynArray:
                if this.arrayVal.len > 0:
                    result = deepcopy this.arrayVal[0]
                    this.arrayVal = this.arrayVal[1..^1]

                    if this.arrayVal.len == 0:
                        this.kind = dynNull
            else:
                result = deepcopy this
                this.kind = dynNull


    proc `>>`*(): self =
        case this.kind:
            of dynArray:
                if this.arrayVal.len > 0:
                    result = this.arrayVal.pop()

                    if this.arrayVal.len == 0:
                        this.kind = dynNull
            else:
                result = deepcopy this
                this.kind = dynNull

    proc `<<`*(that: self): void {. mutator .}=
        if this of nil:
            this = null
        if not(that of nil):
            case this.kind:
                of dynArray:
                    this.arrayVal.add(<< that)
                else:
                    let
                        original = deepcopy this
                    this.kind = dynArray
                    this.arrayVal = newSeq[self]()
                    if original.kind != dynNull:
                        this << original
                    this << that

    proc `>>`*(that: var self): void =
        if that of nil:
            that = null
        if not(this of nil):
            case that.kind:
                of dynArray:
                    that.arrayVal.insert(>> this, 0)
                else:
                    let
                        original = deepcopy that
                    that.kind = dynArray
                    that.arrayVal = newSeq[self]()
                    if original.kind != dynNull:
                        original >> that
                    this >> that

    #
    # Addition
    #

    proc `+`*(that: self): self =
        if this of nil:
            result = null
        if that of nil: # handles cases where a dyn variable was assigned nil and is on the right
            result = deepcopy this
        case this.kind:
            of dynNull:
                result = null
            of dynInt:
                case that.kind:
                    of dynInt:
                        result = ~(this.intVal + that.intVal)
                    of dynFloat:
                        result = ~(float(this.intVal) + that.floatVal) # causes recursion if different
                    of dynString:
                        result = ~(asString(this) & that.stringVal)
                    of dynArray:
                        result = deepcopy that; (deepcopy this) >> result
                    else:
                        discard
            of dynFloat:
                case that.kind:
                    of dynInt:
                        result = ~(this.floatVal + float(that.intVal)) # causes recursion if different
                    of dynFloat:
                        result = ~(this.floatVal + that.floatVal)
                    of dynString:
                        result = ~(asString(this) & that.stringVal)
                    of dynArray:
                        result = deepcopy that; (deepcopy this) >> result
                    else:
                        discard
            of dynString:
                case that.kind:
                    of dynInt, dynFloat, dynString:
                        result = ~(this.stringVal & asString(that))
                    else:
                        discard
            of dynArray:
                result = deepcopy this

                case that.kind:
                    of dynArray:
                        for i in that.arrayVal:
                            result.arrayVal.add(deepcopy i)
                    else:
                        result << (deepcopy that)
            of dynBool:
                case that.kind:
                    of dynBool:
                        result = ~(this.boolVal or that.boolVal)
                    else:
                        result = if this.boolVal: deepcopy that else: deepcopy this
            else:
                discard

        if result.kind == dynEmpty:
            raise newException(
                ValueError,
                fmt "Unsupported operator: {$this.kind} + {$that.kind}"
            )

    #
    # Subtraction
    #

    proc `-`*(that: self): self =
        var
            value = null
        case this.kind:
            of dynInt:
                case that.kind:
                    of dynInt:
                        value = ~(this.intVal - that.intVal)
                    of dynFloat:
                        value = ~(float(this.intVal) - that.floatVal) # causes recursion if different
                    else:
                        discard
            of dynFloat:
                case that.kind:
                    of dynInt:
                        value = ~(this.floatVal - float(that.intVal)) # causes recursion if different
                    of dynFloat:
                        value = ~(this.floatVal - that.floatVal)
                    else:
                        discard
            else:
                discard
        if value.kind == dynNull:
            raise newException(
                ValueError,
                fmt "Unsupported operator for dynamic values: {$this.kind} - {$that.kind}"
            )
        result = value

    #
    # Multiplication
    #

    proc `*`*(that: self): self =
        var
            value = null
        case this.kind:
            of dynInt:
                case that.kind:
                    of dynInt:
                        value = ~(this.intVal * that.intVal)
                    of dynFloat:
                        value = ~(float(this.intVal) * that.floatVal) # causes recursion if different
                    else:
                        discard
            of dynFloat:
                case that.kind:
                    of dynInt:
                        value = ~(this.floatVal * float(that.intVal)) # causes recursion if different
                    of dynFloat:
                        value = ~(this.floatVal * that.floatVal)
                    else:
                        discard
            else:
                discard
        if value.kind == dynNull:
            raise newException(
                ValueError,
                fmt "Unsupported operator for dynamic values: {$this.kind} * {$that.kind}"
            )
        result = value

    #
    # Multiplication
    #

    proc `/`*(that: self): self =
        var
            value = null
        case this.kind:
            of dynInt:
                case that.kind:
                    of dynInt:
                        value = ~(this.intVal / that.intVal)
                    of dynFloat:
                        value = ~(float(this.intVal) / that.floatVal) # causes recursion if different
                    else:
                        discard
            of dynFloat:
                case that.kind:
                    of dynInt:
                        value = ~(this.floatVal / float(that.intVal)) # causes recursion if different
                    of dynFloat:
                        value = ~(this.floatVal / that.floatVal)
                    else:
                        discard
            else:
                discard
        if value.kind == dynNull:
            raise newException(
                ValueError,
                fmt "Unsupported operator for dynamic values: {$this.kind} / {$that.kind}"
            )

        result = value

    #
    # Function handling
    #

    macro register*(name: string, call: untyped): untyped {. static .} =
        var
            minArgc = 0
            maxArgc = 0

        let
            this = newIdentNode("this")
            args = newIdentNode("args")
            decl = newNimNode(nnkProcDef)
            ident = newIdentNode(name.strVal)
            cases = newNimNode(nnkCaseStmt)
            error = newNimNode(nnkElse)
            estmt = newStmtList()

        if call.kind != nnkLambda:
            raise newException(ValueError, "")

        if call[3].kind == nnkFormalParams: # Determine min and max argument count
            for i in 1..<call[3].len:
                inc maxArgc
                if call[3][i][2].kind == nnkEmpty:
                    inc minArgc

        dec minArgc
        dec maxArgc

        cases.add(  # Initialize the case statement
            quote do:
                `args`.len
        )

        for i in minArgc..maxArgc: # Iterate and add cases to determine valid parameter count
            let
                branch = newNimNode(nnkOfBranch)
                code = newStmtList()
                call = quote do:
                    `this`.`ident`()

            branch.add(newIntLitNode(i))

            for j in 0..<i:
                call.add(
                    quote do:
                        `args`[`j`]
                )

            code.add(call)
            branch.add(code)
            cases.add(branch)

        estmt.add( # Add our final error handling
            quote do:
                raise newException(ValueError, fmt "Parameter count out of range")
        )

        error.add(estmt)
        cases.add(error)

        for child in call:
            decl.add(copyNimTree(child))

        decl.del(0)
        decl.insert(0, newNimNode(nnkPostfix))
        decl[0].add(newIdentNode("*"))
        decl[0].add(ident)

        result = quote do: # Construct the wrapped function
            `decl`

            dyn.functions[`name`] = Function(
                minArgc: `minArgc`,
                maxArgc: `maxArgc`,
                call: proc(`this`: dyn, `args`: seq[dyn]): dyn =
                    result = `cases`
            )

    macro function*(call: untyped): untyped {. static .} =
        result = quote do:
            `call`
        discard

    proc hasFunction*(name: string): bool {. static .} =
        result = allFunctions.hasKey(name)

    proc callFunction*(name: string, this: self, args: seq[self]): self {. static .} =
        when defined debug:
            echo fmt "Calling dynamic function '{name}()' on [{$this.kind}]"

        if not allFunctions.hasKey(name):
            raise newException(ValueError, fmt "Invalid function `{name}()` called")

        try:
            result = allFunctions[name].call(this, args)
        except ValueError:
            raise newException(ValueError, fmt "Cannot call `{name}() with {args.len} parameters, wrong count`")

    proc functions*(): var FunctionRegistry {. static .} =
        result = allFunctions

    #
    # Available functions
    #

    self.register(
        "len",
        proc(this: self): self =
            if this of nil:
                result = -1
            elif this of array:
                result = asArray(this).len
            elif this of object:
                result = asObject(this).len
            elif this of bool:
                result = if this: 1 else: 0
            elif this of int:
                var
                    value: string
                    sign: int = 1
                if this < 0:
                    value = asString(this)[1..^1]
                    sign = -1
                else:
                    value = asString(this)

                result = sign * asInt(value.len)
            elif this of float:
                var
                    value: string
                    sign: int = 1
                if this < 0:
                    value = asString(this)[1..^1]
                    sign = -1
                else:
                    value = asString(this)
                let
                    split = value.find('.')

                result = sign * asFloat($split & "." & $(value.len - (split + 1)))
            else:
                result = asString(this).len
    )

    self.register(
        "has",
        proc(this: self, query: self): self =
            result = false
            if this of null:
                result = false
            elif this of array:
                result = asArray(this).contains(query)
            elif this of object:
                let
                    query = asString(query)
                    concern = asObject(this)
                if concern.hasKey(query):
                    result = concern[query] != null
            else:
                result = asString(this).find(query) >= 0
    )
