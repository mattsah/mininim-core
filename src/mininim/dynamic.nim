{. experimental: "dotOperators" .}

import
    mininim,
    std/parseutils

type
    dynType* = enum
        dynInt,
        dynFloat,
        dynString,
        dynObject,
        dynArray,
        dynBool,
        dynNull

    dyn* = ref object of Class
        kind: dynType
        intVal: int
        floatVal: float
        stringVal: string
        objectVal: Table[string, dyn]
        arrayVal: seq[dyn]
        boolVal: bool

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
converter toDyn*(this: typeof(nil)): dyn =
    when defined debug:
        echo fmt "Converting [nil] to dynamic value"
    result = dyn(kind: dynNull)

converter toDyn*(this: int): dyn =
    when defined debug:
        echo fmt "Converting [int] to dynamic value"
    result = dyn(kind: dynInt, intVal: this)

converter toDyn*(this: float): dyn =
    when defined debug:
        echo fmt "Converting [float] to dynamic value"
    result = dyn(kind: dynFloat, floatVal: this)

converter toDyn*(this: bool): dyn =
    when defined debug:
        echo fmt "Converting [bool] to dynamic value"
    result = dyn(
        kind: dynBool,
        boolVal: this
    )

converter toDyn*(this: string): dyn =
    when defined debug:
        echo fmt "Converting [string] to dynamic value"
    result = dyn(kind: dynString, stringVal: this)

converter toDyn*[T: auto](this: openArray[T]): dyn =
    when defined debug:
        echo fmt "Converting [openArray[{$T}]] to dynamic value"
    result = dyn(
        kind: dynArray,
        arrayVal: this.mapIt(toDyn(it))
    )

converter toDyn*[T: openArray](this: T): dyn =
    when defined debug:
        echo fmt "Converting [string] to dynamic value"
    result = dyn(
        kind: dynArray,
        arrayVal: newSeq[dyn]()
    )

converter toDyn*(this: tuple): dyn =
    when defined debug:
        echo fmt "Converting [tuple] to dynamic value"
    var
        cur = 0
        table = false
        value = newSeq[(string, dyn)]()
    if this.tupleLen == 0:
        table = true
    else:
        for key, item in this.fieldPairs:
            if key != "Field" & $cur:
                table = true
            value.add((key, toDyn(item)))
            inc cur
    if table:
        result = dyn(
            kind: dynObject,
            objectVal: value.toTable
        )
    else:
        result = dyn(
            kind: dynArray,
            arrayVal: value.mapIt(it[1])
        )

converter toDyn*[K: auto, T: auto](this: Table[K, T]): dyn =
    when defined debug:
        echo fmt "Converting [Table[{$K}, {$T}]] to dynamic value"
    var
        value = initTable[string, dyn]()
    for key, item in this.pairs:
        value[$key] = toDyn(item)
    result = dyn(
        kind: dynObject,
        objectVal: value
    )

#[
    Convert basically anything to a dynamic value
]#
macro `~`*(value: untyped): dyn =
    var
        arrayVal: NimNode

    if value.kind == nnkBracket:
        arrayVal = value
    elif value.kind == nnkPrefix and value[0].strVal == "@" and value[1].kind == nnkBracket:
        arrayVal = value[1]

    if arrayVal != nil and arrayVal.len == 0:
        result = quote do:
            dyn(kind: dynArray, arrayVal: newSeq[dyn]())
    elif value.kind == nnkBracketExpr:
        let
            dyn = value[0]
            key = value[1]
        result = quote do:
            toDyn(`dyn`)[`key`]
    else:
        result = quote do:
            toDyn(`value`)

template `:=`*(this: untyped, value: untyped): untyped =
    var this = ~value

template `~@`*(value: untyped): untyped =
    ~ value

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

        when T == int:
            result = ~toInt(this)
        when T == float:
            result = ~toFloat(this)
        when T == string:
            result = ~toString(this)
        when T == array:
            result = ~toArray(this)
        when T == object:
            result = ~toObject(this)
        when T == bool:
            result = ~toBool(this)

    proc `of`*(that: typeof(nil)): bool =
        if this == nil:
            result = true
        else:
            result = this.kind == dynNull

    proc `of`*[T](that: typedesc[T]): bool =
        result = false

        when T == int:
            result = this.kind == dynInt
        when T == float:
            result = this.kind == dynFloat
        when T == string:
            result = this.kind == dynString
        when T == array:
            result = this.kind == dynArray
        when T == object:
            result = this.kind == dynObject
        when T == bool:
            result = this.kind == dynBool
        when T == dyn:
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

    converter toDyn*(): dyn =
        if this == nil:
            result = toDyn(nil)
        else:
            result = this

    converter toInt*(): int =
        when defined debug:
            echo fmt "Converting dynamic value [{this.kind}] to int"
        case this.kind:
            of dynInt:
                result = this.intVal
            of dynFloat:
                result = int(this.floatVal)
            of dynString:
                if this.stringVal.parseInt(result) != this.stringVal.len:
                    result = 0
            else:
                raise newException(ValueError, fmt "Cannot convert [{this.kind}] to int")

    converter toFloat*(): float =
        when defined debug:
            echo fmt "Converting dynamic value [{this.kind}] to float"
        case this.kind:
            of dynInt:
                result = float(this.intVal)
            of dynFloat:
                result = this.floatVal
            of dynString:
                if this.stringVal.parseFloat(result) != this.stringVal.len:
                    result = 0
            else:
                raise newException(ValueError, fmt "Cannot convert [{this.kind}] to float")

    converter toString*(): string =
        when defined debug:
            echo fmt "Converting dynamic value [{this.kind}] to string"
        if this of nil:
            result = "null"
        else:
            case this.kind:
                of dynInt:
                    result = $this.intVal
                of dynFloat:
                    result = $this.floatVal
                of dynString:
                    result = this.stringVal
                of dynArray:
                    result = '[' & this.arrayVal.join(",") & ']'
                of dynObject:
                    result = $this.objectVal
                of dynBool:
                    result = if this.boolVal: "true" else: "false"
                else:
                    discard

    converter toBool*(): bool =
        when defined debug:
            echo fmt "Converting dynamic value [{this.kind}] to bool"
        if this of nil:
            return false
        else:
            case this.kind:
                of dynInt:
                    result = this.intVal == 0
                of dynFloat:
                    result = this.floatVal == 0
                of dynString:
                    result = this.stringVal.len > 0
                of dynArray:
                    result = this.arrayVal.len > 0
                of dynObject:
                    result = this.objectVal.len > 0
                of dynBool:
                    result = this.boolVal
                else:
                    discard

    converter toArray*(): seq[self] =
        when defined debug:
            echo fmt "Converting dynamic value [{this.kind}] to array (sequence)"
        if this of nil:
            result = newSeq[self]()
        else:
            case this.kind:
                of dynArray:
                    result = this.arrayVal
                of dynObject:
                    result = toSeq(this.objectVal.values)
                else:
                    result = @[this]

    converter toObject*(): Table[string, self] =
        when defined debug:
            echo fmt "Converting dynamic value [{this.kind}] to object (table)"
        case this.kind:
            of dynObject:
                result = this.objectVal
            else:
                result = initTable[string, self]()
                result["value"] = copy this

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
            result = toString(this)

    #[
        Shorthand for sequence conversion
    ]#
    proc `@`*(): seq[self] =
        if this == nil: # Handled explicitly as you can't pass along null ref
            result = newSeq[self]()
        else:
            result = toArray(this)

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
                    key = toString(key)

                if this.objectVal.hasKey(key):
                    result = this.objectVal[key]
                else:
                    raise newException(
                        ValueError,
                        fmt "Accessing '{key}' failed, value not defined"
                    )
            of dynArray:
                var
                    key = toInt(key)
                if key < 0:
                    key = this.arrayVal.len - key
                if key <= this.arrayVal.high and key >= this.arrayVal.low:
                    result = this.arrayVal[key]
                else:
                    raise newException(
                        ValueError,
                        fmt "Accessing '{key}' failed, outside range"
                    )
            of dynString:
                var
                    key = toInt(key)
                if key < 0:
                    key = this.stringVal.len - key
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
                    key = toString(key)

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
                    key = toInt(key)

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

    #[
        Equal
    ]#

    proc `==`*(that: self): bool =
        result = false

        if this of nil:
            result = that.kind == dynNull
        elif that of nil:
            result = this.kind == dynNull
        else:
            case this.kind:
                of dynInt:
                    result = this.intVal == toInt(that)
                of dynFloat:
                    result = this.floatVal == toFloat(that)
                of dynString:
                    result = this.stringVal == toString(that)
                of dynBool:
                    result = this.boolVal == toBool(that)
                else:
                    discard

    proc `==`*(that: auto): bool =
        result = this == toDyn(that)

    proc `==`*(that: auto): bool {. infix .}=
        result = this == toDyn(that)

    #[
        Not Equal
    ]#
    proc `!=`*(that: self): bool =
        result = false

        if this of nil:
            result = that.kind != dynNull
        elif that of nil:
            result = this.kind != dynNull
        else:
            case this.kind:
                of dynInt:
                    result = this.intVal != toInt(that)
                of dynFloat:
                    result = this.floatVal != toFloat(that)
                of dynString:
                    result = this.stringVal != toString(that)
                of dynBool:
                    result = this.boolVal != toBool(that)
                else:
                    discard


    proc `!=`*(that: auto): bool =
        result = this != toDyn(that)

    proc `!=`*(that: auto): bool {. infix .}=
        result = this != toDyn(that)

    #[
        Greater Than or Equal
    ]#
    proc `>=`*(that: self): bool =
        result = false

        if this of nil:
            result = that.kind == dynNull or false
        elif that of nil:
            result = this.kind == dynNull or true
        else:
            case this.kind:
                of dynInt:
                    result = this.intVal >= toInt(that)
                of dynFloat:
                    result = this.floatVal >= toFloat(that)
                of dynString:
                    result = this.stringVal >= toString(that)
                of dynBool:
                    result = this.boolVal >= toBool(that)
                else:
                    discard

    proc `>=`*(that: auto): bool =
        result = this >= toDyn(that)

    proc `>=`*(that: auto): bool {. infix .}=
        result = this >= toDyn(that)


    #[
        Less Than or Equal
    ]#
    proc `<=`*(that: self): bool =
        result = false

        if this of nil:
            result = that.kind == dynNull or true
        elif that of nil:
            result = this.kind == dynNull or false
        else:
            case this.kind:
                of dynInt:
                    result = this.intVal >= toInt(that)
                of dynFloat:
                    result = this.floatVal >= toFloat(that)
                of dynString:
                    result = this.stringVal >= toString(that)
                of dynBool:
                    result = this.boolVal >= toBool(that)
                else:
                    discard

    proc `<=`*(that: auto): bool =
        result = this <= toDyn(that)

    proc `<=`*(that: auto): bool {. infix .}=
        result = this <= toDyn(that)

    #[

    ]#
    proc `and`*(that: auto): bool =
        if this of nil:
            result = toBool(null) and that
        else:
            result = toBool(this) and that # convert known dyn value to force second if needed

    proc `or`*(that: auto): bool =
        if this of nil:
            result = toBool(null) or that
        else:
            result = toBool(this) or that # convert known dyn value to force second if needed

    proc `not`*(): bool =
        if this of nil:
            result = true
        else:
            case this.kind:
                of dynBool: # optimize for bools directly
                    result = this.boolVal
                else: # retain toBool centralized logic for all others
                    result = not toBool(this)

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
        var
            value = null

        if that of nil: # handles cases where a dyn variable was assigned nil and is on the right
            value = deepcopy this
        else:
            case this.kind:
                of dynInt:
                    case that.kind:
                        of dynInt:
                            value = ~(this.intVal + that.intVal)
                        of dynFloat:
                            value = ~(float(this.intVal) + that.floatVal) # causes recursion if different
                        of dynString:
                            value = ~(toString(this) & that.stringVal)
                        of dynArray:
                            value = deepcopy that; (deepcopy this) >> value
                        else:
                            discard
                of dynFloat:
                    case that.kind:
                        of dynInt:
                            value = ~(this.floatVal + float(that.intVal)) # causes recursion if different
                        of dynFloat:
                            value = ~(this.floatVal + that.floatVal)
                        of dynString:
                            value = ~(toString(this) & that.stringVal)
                        of dynArray:
                            value = deepcopy that; (deepcopy this) >> value
                        else:
                            discard
                of dynString:
                    case that.kind:
                        of dynInt, dynFloat, dynString:
                            value = ~(this.stringVal & toString(that.toString))
                        else:
                            discard
                of dynArray:
                    value = deepcopy this

                    case that.kind:
                        of dynArray:
                            for i in that.arrayVal:
                                value.arrayVal.add(deepcopy i)
                        else:
                            value << (deepcopy that)
                of dynBool:
                    case that.kind:
                        of dynBool:
                            value = ~(this.boolVal or that.boolVal)
                        else:
                            value = if this.boolVal: deepcopy that else: deepcopy this
                else:
                    discard

        if value.kind == dynNull and this.kind != dynNull:
            raise newException(
                ValueError,
                fmt "Unsupported operator: {$this.kind} + {$that.kind}"
            )

        result = value

    proc `+`*(that: self): self {. mutator .} =
        if this of nil: # handles cases where a dyn variable was assigned nil and is on the left
            result = null + that
        else:
            result = this + that

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
                        value = ~(this.floatVal * float(this.intVal)) # causes recursion if different
                    of dynFloat:
                        value = ~(this.floatVal * that.floatVal)
                    else:
                        discard
            else:
                discard
        if value.kind != dynNull:
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
        if value.kind != dynNull:
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
            echo fmt "Calling dynamic function {name} on {$this}"

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
                result = toArray(this).len
            elif this of object:
                result = toObject(this).len
            elif this of bool:
                result = if this: 1 else: 0
            elif this of float:
                let
                    value = toString(this)
                    start = value.find('.')

                result = toFloat($start & "." & $(value.len - (start + 1)))
            else:
                result = this.toString.len
    )

    self.register(
        "has",
        proc(this: self, query: self): self =
            result = false

            if this of string:
                result = toString(this).find(query) >= 0
            elif this of array:
                result = toArray(this).contains(query)
            elif this of object:
                let
                    concern = toObject(this)
                if concern.hasKey(query):
                    result = concern[query] as bool
            else:
                result = this == query
    )
