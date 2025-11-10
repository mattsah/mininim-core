{. experimental: "dotOperators" .}

import
    mininim

type
    dyn* = ref object of Class
        value: JsonNode = newJNull()

    FunctionRegistry* = Table[string, Function]
    FunctionWrapper* = proc(this: dyn, args: seq[dyn]): dyn {. closure .}
    Function* = ref object of Class
        minArgc*: int
        maxArgc*: int
        call*: FunctionWrapper

var
    allFunctions = FunctionRegistry()

let
    null* = dyn()

#[
    Converters to dyn type
]#
converter toDyn*(this: JsonNode): dyn =
    if this.isNil:
        when defined debug:
            echo fmt "Converting [nil] to dynamic value"
        result = dyn()
    else:
        when defined debug:
            echo fmt "Converting [{this.kind}] {$this} to dynamic value"
        result = dyn(value: this)

converter toDyn*(this: int): dyn =
    when defined debug:
        echo fmt "Converting [int] {$this} to dynamic value"
    result = dyn(value: % this)

converter toDyn*(this: float): dyn =
    when defined debug:
        echo fmt "Converting [float] {$this} to dynamic value"
    result = dyn(value: % this)

converter toDyn*(this: string): dyn =
    when defined debug:
        echo fmt "Converting [string] '{this}' to dynamic value"
    result = dyn(value: % this)

converter toDyn*(this: bool): dyn =
    when defined debug:
        echo fmt "Converting [bool] '{$this}' to dynamic value"
    result = dyn(value: % this)

converter toDyn*[T: auto, N: int](this: array[N, T]): dyn =
    result = dyn(value: % this.toSeq)

converter toDyn*[T](this: openArray[T]): dyn =
    when defined debug:
        echo fmt "Converting [openArray[{$T}]] {$this} to dynamic value"
    result = dyn(value: % this)

converter toDyn*(this: tuple): dyn =
    var
        current = 0

    let
        json = % this
        values = newJArray()

    when defined debug:
        echo fmt "Converting [tuple] {$this} to dynamic value"
    block translate:
        for key, value in json:
            echo key
            if key != "Field" & $current:
                result = dyn(value: json)
                break translate;

            values.add(value)
            inc current
        result = dyn(value: values)

#[
    Convert basically anything to a dynamic value
]#
template `~`*(this: untyped): dyn =
    this.toDyn

#[
    Macros
]#
macro `:=`*(this: untyped, value: untyped): untyped =
    if value.kind == nnkBracket and value.len == 0:
        result = quote do:
            var `this` = toDyn(newJArray())
    else:
        result = quote do:
            var `this` = toDyn(`value`)

begin dyn:
    converter toJson*(): JsonNode =
        result = copy this.value

    converter toInt*(): int =
        when defined debug:
            echo fmt "Converting dynamic value {$this} to int"

        case this.value.kind:
            of JInt:
                result = this.value.to(int)
            of JFloat:
                result = int(this.value.to(float))
            else:
                raise newException(ValueError, fmt "Cannot convert {this.value.kind} to integer")

    converter toFloat*(): float =
        when defined debug:
            echo fmt "Converting dynamic value {$this} to float"

        case this.value.kind:
            of JInt:
                result = float(this.value.to(int))
            of JFloat:
                result = this.value.to(float)
            else:
                raise newException(ValueError, fmt "Cannot convert {this.value.kind} to float")

    converter toString*(): string =
        when defined debug:
            echo fmt "Converting dynamic value {$this} to string"

        case this.value.kind:
            of JString:
                result = this.value.to(string)
            else:
                result = $this.value

    converter toBool*(): bool =
        when defined debug:
            echo fmt "Converting dynamic value {$this} to bool"

        case this.value.kind:
            of JBool:
                result = this.value.to(bool)
            else:
                result = false

    converter toSeq*(): seq[self] =
        when defined debug:
            echo fmt "Converting dynamic value {$this} to dynamic sequence"

        case this.value.kind:
            of JNull:
                result = newSeq[self]()
            of JArray:
                result = this.value.mapIt(it.toDyn)
            else:
                result = @[this]

    #[
        Shorthand for string conversion
    ]#
    proc `$`*(): string =
        result = this.toString

    #[
        Shorthand for json conversion
    ]#
    proc `%`*(): JsonNode =
        result = this.toJson

    #[
        Explicitly ensure existing dynamic types are not re-converted
    ]#
    proc `~`*(): self =
        result = this

    #[
        Shorthand for sequence conversion
    ]#
    proc `@`*(): seq[self] =
        result = this.toSeq


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
        var
            equal: bool = false

        case this.value.kind:
            of JNull:
                case that.value.kind:
                    of JNull:
                        equal = true
                    else:
                        discard
            else:
                discard


        result = self(value: % equal)

    proc `==`*(that: auto): self =
        result = this == self(value: % that)

    proc `==`*(that: auto): self {. infix .} =
        result = this == self(value: % that)

    #
    # Addition
    #

    proc `+`*(that: self): self =
        var
            value = self()

        case this.value.kind:
            of JInt:
                case that.value.kind:
                    of JInt:
                        value = self(value: %(int(this) + int(that)))
                    of JFloat:
                        value = self(value: %(float(this) + float(that))) # causes recursion if different
                    of JString:
                        value = self(value: %(string(this) & string(that)))
                    else:
                        discard
            of JFloat:
                case that.value.kind:
                    of JInt:
                        value = self(value: %(float(this) + float(that)))
                    of JFloat:
                        value = self(value: %(float(this) + float(that))) # causes recursion if different
                    of JString:
                        value = self(value: %(string(this) & string(that)))
                    else:
                        discard
            of JString:
                case that.value.kind:
                    of JInt, JFloat, JString:

                        value = self(value: %(string(this) & string(that)))
                    else:
                        discard
            of JArray:
                value = self(value: copy this.value)

                case that.value.kind:
                    of JArray:
                        for i in that.value:
                            value.add(copy i)
                    else:
                        value.add(that.value)
            else:
                discard

        if value == null:
            raise newException(
                ValueError,
                "Unsupported operator '+' for dynamic value: " & astToStr(this)
            )

        result = value

    proc `+`*(that: auto): self =
        result = this + self(value: % that)

    #
    # Subtraction
    #

    proc `-`*(that: self): self =
        var
            value = self()

        case this.value.kind:
            of JInt:
                case that.value.kind:
                    of JInt:
                        value = self(value: %(int(this) - int(that)))
                    of JFloat:
                        value = self(value: %(float(this) - float(that))) # causes recursion if different
                    of JString:
                        value = self(value: %(string(this) & string(that)))
                    else:
                        discard
            of JFloat:
                case that.value.kind:
                    of JInt:
                        value = self(value: %(float(this) - float(that)))
                    of JFloat:
                        value = self(value: %(float(this) - float(that))) # causes recursion if different
                    of JString:
                        value = self(value: %(string(this) & string(that)))
                    else:
                        discard
            of JString:
                case that.value.kind:
                    of JInt, JFloat, JString:
                        value = self(value: %(string(this) & string(that)))
                    else:
                        discard
            of JArray:
                value = self(value: copy this.value)

                case that.value.kind:
                    of JArray:
                        for i in that.value:
                            value.add(copy i)
                    else:
                        value.add(that.value)
            else:
                discard

        if value == null:
            raise newException(
                ValueError,
                "Unsupported operator '+' for dynamic value: " & astToStr(this)
            )

        result = value

    proc `-`*(that: auto): self =
        result = this - self(value: % that)

    #
    # Multiplication
    #

    proc `*`*(that: self): self =
        var
            value = self()

        case this.value.kind:
            of JInt:
                case that.value.kind:
                    of JInt:
                        value = self(value: %(int(this) * int(that)))
                    of JFloat:
                        value = self(value: %(float(this) * float(that))) # causes recursion if different
                    of JString:
                        value = self(value: %(string(this) & string(that)))
                    else:
                        discard
            of JFloat:
                case that.value.kind:
                    of JInt:
                        value = self(value: %(float(this) * float(that)))
                    of JFloat:
                        value = self(value: %(float(this) * float(that))) # causes recursion if different
                    of JString:
                        value = self(value: %(string(this) & string(that)))
                    else:
                        discard
            of JString:
                case that.value.kind:
                    of JInt, JFloat, JString:
                        value = self(value: %(string(this) & string(that)))
                    else:
                        discard
            of JArray:
                value = self(value: copy this.value)

                case that.value.kind:
                    of JArray:
                        for i in that.value:
                            value.add(copy i)
                    else:
                        value.add(that.value)
            else:
                discard

        if value == null:
            raise newException(
                ValueError,
                "Unsupported operator '+' for dynamic value: " & astToStr(this)
            )

        result = value

    proc `*`*(that: auto): self =
        result = this * self(value: % that)

    #
    # Multiplication
    #

    proc `/`*(that: self): self =
        var
            value = self()

        case this.value.kind:
            of JInt:
                case that.value.kind:
                    of JInt:
                        value = self(value: %(int(this) / int(that)))
                    of JFloat:
                        value = self(value: %(float(this) / float(that))) # causes recursion if different
                    of JString:
                        value = self(value: %(string(this) & string(that)))
                    else:
                        discard
            of JFloat:
                case that.value.kind:
                    of JInt:
                        value = self(value: %(float(this) / float(that)))
                    of JFloat:
                        value = self(value: %(float(this) / float(that))) # causes recursion if different
                    of JString:
                        value = self(value: %(string(this) & string(that)))
                    else:
                        discard
            of JString:
                case that.value.kind:
                    of JInt, JFloat, JString:
                        value = self(value: %(string(this) & string(that)))
                    else:
                        discard
            of JArray:
                value = self(value: copy this.value)

                case that.value.kind:
                    of JArray:
                        for i in that.value:
                            value.add(copy i)
                    else:
                        value.add(that.value)
            else:
                discard

        if value == null:
            raise newException(
                ValueError,
                "Unsupported operator '+' for dynamic value: " & astToStr(this)
            )

        result = value

    proc `/`*(that: auto): self =
        result = this / self(value: % that)

    #
    # Access
    #

    proc `<<`*(): self =
        case this.value.kind:
            of JArray:
                if this.value.len > 0:
                    result = this.value[0]
                    this.value = this.value[1..^1]
                else:
                    raise newException(KeyError, "Cannot shift `<<` on empty array")
            else:
                result = self()
                result.value = this.value
                this.value = newJNull()

    proc `>>`*(): self =
        case this.value.kind:
            of JArray:
                if this.value.len > 0:
                    result = this.value.elems.pop()
                else:
                    raise newException(KeyError, "Cannot shift `>>` on empty array")

            else:
                result = this.value
                this.value = newJNull()

    proc `<<`*(that: self): void =
        case this.value.kind:
            of JNull:
                this.value = % << that
            of JArray:
                this.value.add(% << that)
            else:
                this.value = % [this.value, % << that]

    proc `>>`*(that: self): void =
        case this.value.kind:
            of JNull:
                that.value = % >> this
            of JArray:
                that.value.elems.insert(% >> this, 0)
            else:
                that.value = % [% >> this, that.value]

    proc `[]`*(field: string): self =
        case this.value.kind:
            of JObject:
                if this.value.hasKey(field):
                    result = this.value[field]
                else:
                    result = newJNull()

            else:
                raise newException(
                    ValueError,
                    fmt "Cannot read property '{field}' from non object/array data"
                )

    proc `[]=`*(key: string, value: auto): void =
        case this.value.kind:
            of JObject:
                when value is self:
                    if null == value:
                        if this.value.hasKey(key):
                            this.value.delete(key)
                    else:
                        this.value[key] = value

                elif value is JsonNode:
                    this.value[key] = copy value

                else:
                    this.value[key] = % value
            else:
                raise newException(ValueError, fmt "Cannot assign property to non object/array data")

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

    self.register(
        "join",
        proc(separator: string = ""): self =
            echo this.value.kind
            result = @this.join(separator)
    )
