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
        case kind: dynType
            of dynInt:
                intVal: int
            of dynFloat:
                floatVal: float
            of dynString:
                stringVal: string
            of dynObject:
                objectVal: Table[string, dyn]
            of dynArray:
                arrayVal: seq[dyn]
            of dynBool:
                boolVal: bool
            of dynNull:
                discard

    FunctionRegistry* = Table[string, Function]
    FunctionWrapper* = proc(this: dyn, args: seq[dyn]): dyn {. closure .}
    Function* = ref object of Class
        minArgc*: int
        maxArgc*: int
        call*: FunctionWrapper

const
    empty* = []

let
    null* = dyn(kind: dynNull)

var
    allFunctions = FunctionRegistry()


#[
    Converters to dyn type
]#
converter toDyn*(this: Class): dyn =
    if this == nil:
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

converter toDyn*(this: string): dyn =
    var
        intVal: int
        floatVal: float
    when defined debug:
        echo fmt "Converting [string] to dynamic value"
    if this.len > 0 and this.parseInt(intVal) == this.len:
        result = dyn(kind: dynInt, intVal: intVal)
    elif this.len > 0 and this.parseFloat(floatVal) == this.len:
        result = dyn(kind: dynFloat, floatVal: floatVal)
    else:
        result = dyn(kind: dynString, stringVal: this)

converter toDyn*(this: bool): dyn =
    when defined debug:
        echo fmt "Converting [bool] to dynamic value"
    result = dyn(kind: dynBool, boolVal: this)

converter toDyn*[T, N: int](this: array[N, T]): dyn =
    when defined debug:
        echo fmt "Converting [array[{$N}, {$T}]] to dynamic value"
    result = dyn(kind: dynArray, arrayVal: this.mapIt(toDyn(it)))

converter toDyn*[T](this: openArray[T]): dyn =
    when defined debug:
        echo fmt "Converting [openArray[{$T}]] to dynamic value"
    result = dyn(kind: dynArray, arrayVal: this.mapIt(toDyn(it)))

converter toDyn*(this: tuple): dyn =
    var
        cur = 0
        table = false
        value = newSeq[(string, dyn)]()
    when defined debug:
        echo fmt "Converting [tuple] to dynamic value"
    for key, item in this.fieldPairs:
        if key != "Field" & $cur:
            table = true

        value.add((key, toDyn(item)))
        inc cur

    if table:
        result = dyn(kind: dynObject, objectVal: value.toTable)
    else:
        result = dyn(kind: dynArray, arrayVal: value.mapIt(it[1]))

#[
    Convert basically anything to a dynamic value
]#

macro `:=`*(this: untyped, value: untyped): untyped =
    if value.kind == nnkBracket and value.len == 0:
        result = quote do:
            var `this` = toDyn(newSeq[dyn]())
    else:
        result = quote do:
            var `this` = toDyn(`value`)

macro `~`*(value: untyped): dyn =
    if value.kind == nnkBracket and value.len == 0:
        result = quote do:
            toDyn(newSeq[dyn]())
    else:
        result = quote do:
            toDyn(`value`)

#[
    Primary dyn implementation
]#

begin dyn:
    proc `$`*(): string # Forward declaration for debug messages

    converter toDyn*(): dyn =
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
            of dynNull:
                result = "null"

    converter toBool*(): bool =
        when defined debug:
            echo fmt "Converting dynamic value [{this.kind}] to bool"
        case this.kind:
            of dynBool:
                result = this.boolVal
            else:
                result = false

    converter toSeq*(): seq[self] =
        when defined debug:
            echo fmt "Converting dynamic value [{this.kind}] to dynamic sequence"
        case this.kind:
            of dynNull:
                result = newSeq[self]()
            of dynArray:
                result = this.arrayVal
            else:
                result = @[this]

    #[
        Shorthand for string conversion
    ]#
    proc `$`*(): string =
        result = toString(this)

    #[
        Shorthand for sequence conversion
    ]#
    proc `@`*(): seq[self] =
        result = toSeq(this)


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
            inner = newIdentNode("inner")
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

        cases.add(  # Initialize the case statement
            quote do:
                `args`.len
        )

        for i in minArgc..maxArgc: # Iterate and add cases to determine valid parameter count
            let
                branch = newNimNode(nnkOfBranch)
                code = newStmtList()
                call = newCall(inner)

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

        result = quote do: # Construct the wrapped function
            dyn.functions[`name`] = Function(
                minArgc: `minArgc`,
                maxArgc: `maxArgc`,
                call: proc(`this`: dyn, `args`: seq[dyn]): dyn =
                    let
                        `inner` = `call`

                    result = `cases`
            )

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
    # Equality
    #

    proc `==`*(that: self): self =
        result = dyn(kind: dynBool, boolVal: false)

        case this.kind:
            of dynNull:
                case that.kind:
                    of dynNull:
                        result.boolVal = true
                    else:
                        discard
            else:
                discard

    proc `==`*(that: auto): self =
        result = this == toDyn(that)

    proc `==`*(that: auto): self {. infix .} =
        result = this == toDyn(that)

    #
    # Addition
    #

    proc `+`*(that: self): self =
        var
            value = null
        case this.kind:
            of dynInt:
                case that.kind:
                    of dynInt:
                        value = ~(this.intVal + that.intVal)
                    of dynFloat:
                        value = ~(float(this.intVal) + that.floatVal) # causes recursion if different
                    of dynString:
                        value = ~(this.toString & that.stringVal)
                    else:
                        discard
            of dynFloat:
                case that.kind:
                    of dynInt:
                        value = ~(this.floatVal + float(that.intVal)) # causes recursion if different
                    of dynFloat:
                        value = ~(this.floatVal + that.floatVal)
                    of dynString:
                        value = ~(this.toString & that.stringVal)
                    else:
                        discard
            of dynString:
                case that.kind:
                    of dynInt, dynFloat, dynString:
                        value = ~(this.stringVal & that.toString)
                    else:
                        discard
            of dynArray:
                value = deepCopy this

                case that.kind:
                    of dynArray:
                        for i in that.arrayVal:
                            value.arrayVal.add(deepCopy i)
                    else:
                        value.arrayVal.add(deepCopy that)
            else:
                discard

        if value == null:
            raise newException(
                ValueError,
                fmt "Unsupported operator for dynamic values: {$this.kind} + {$that.kind}"
            )

        result = value

    proc `+`*(that: auto): self =
        result = this + toDyn(that)

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
        if value == null:
            raise newException(
                ValueError,
                fmt "Unsupported operator for dynamic values: {$this.kind} - {$that.kind}"
            )
        result = value

    proc `-`*(that: auto): self =
        result = this - toDyn(that)

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
        if value == null:
            raise newException(
                ValueError,
                fmt "Unsupported operator for dynamic values: {$this.kind} * {$that.kind}"
            )
        result = value


    proc `*`*(that: auto): self =
        result = this * toDyn(that)

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
        if value == null:
            raise newException(
                ValueError,
                fmt "Unsupported operator for dynamic values: {$this.kind} / {$that.kind}"
            )

        result = value


    proc `/`*(that: auto): self =
        result = this / toDyn(that)

    #
    # Access and Operators
    #

    proc `<<`*(): self =
        case this.kind:
            of dynArray:
                if this.arrayVal.len > 0:
                    result = deepCopy this.arrayVal[0]
                    this.arrayVal = this.arrayVal[1..^1]
                else:
                    raise newException(KeyError, "Cannot shift `<<` on empty array")
            else:
                result = deepCopy this


    proc `>>`*(): self =
        case this.kind:
            of dynArray:
                if this.arrayVal.len > 0:
                    result = this.arrayVal.pop()
                else:
                    raise newException(KeyError, "Cannot shift `>>` on empty array")
            else:
                result = deepCopy this

    proc `<<`*(that: self): void=
        case this.kind:
            of dynArray:
                this.arrayVal.add(<< that)
            else:
                raise newException(KeyError, "Cannot shift `<<` onto a non-array")

                # let
                #     original = deepCopy this

                # reset(this)
                # this.kind = dynArray
                # this.arrayVal.add(original)
                # this.arrayVal.add(<< that)

    proc `>>`*(that: self): void =
        case this.kind:
            of dynArray:
                that.arrayVal.insert(>> this, 0)
            else:
                raise newException(KeyError, "Cannot shift `>>` onto a non-array")

                # let
                #     original = deepCopy that

                # reset(that)
                # that.kind = dynArray
                # this.arrayVal.add(original)
                # this.arrayVal.add(>> this)

    proc `[]`*(field: string): self =
        case this.kind:
            of dynObject:
                if this.objectVal.hasKey(field):
                    result = this.objectVal[field]
                else:
                    raise newException(
                        ValueError,
                        fmt "Accessing '{field}' failed, value not defined"
                    )

            else:
                raise newException(
                    ValueError,
                    fmt "Cannot read property '{field}' from non object/array data"
                )

    proc `[]=`*(key: string, value: auto): void =
        case this.kind:
            of dynObject:
                when value is self:
                    if value == null:
                        if this.objectVal.hasKey(key):
                            this.objectVal.del(key)
                    else:
                        this.objectVal[key] = value
                else:
                    this.objectVal[key] = value
            else:
                raise newException(
                    ValueError,
                    fmt "Cannot assign property to non object/array data"
                )

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

    #[

    ]#
    proc `of`*(that: Class): self =
        if that == nil:
            result = this.kind == dynNull
        else:
            result = false

    proc `of`*(that: dynType): self =
        result = this.kind == that

    proc `of`*[T](that: typedesc[T]): self =
        result = false

        when T == int:
            result = this.kind == dynInt
        when T == float:
            result = this.kind == dynFloat
        when T == string:
            result = this.kind == dynFloat
        when T == array:
            result = this.kind == dynArray
        when T == object:
            result = this.kind == dynObject
        when T == bool:
            result = this.kind == dynBool
        when T == dyn:
            result = true

    #
    # Available functions
    #

    self.register(
        "join",
        proc(separator: string = ""): self =
            result = @this.join(separator) # Convert explicitly to a sequence before joining
    )

    self.register(
        "len",
        proc(): self =
            result = this.toString.len
    )
