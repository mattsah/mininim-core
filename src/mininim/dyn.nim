{. experimental: "dotOperators" .}

import
    mininim

type
    dyn* = ref object of Class
        value: JsonNode = newJNull()

let
    null* = dyn()


#[
    Converters to dyn type
]#
converter toDyn*(this: JsonNode): dyn =
    result = dyn(value: this)

converter toDyn*(this: int): dyn =
    result = dyn(value: % this)

converter toDyn*(this: float): dyn =
    result = dyn(value: % this)

converter toDyn*(this: string): dyn =
    result = dyn(value: % this)

converter toDyn*(this: bool): dyn =
    result = dyn(value: % this)

converter toDyn*(this: tuple): dyn =
    result = dyn(value: % this)

#[
    Macros
]#
macro `:=`*(this: untyped, value: untyped): untyped =
    result = quote do:
        var `this` = dyn(value: %(`value`))

begin dyn:
    converter toJson*(): JsonNode =
        result = copy this.value

    converter toInt*(): int =
        case this.value.kind:
            of JInt:
                result = this.value.to(int)
            of JFloat:
                result = int(this.value.to(float))
            else:
                raise newException(ValueError, fmt "Cannot convert {this.value.kind} to integer")

    converter toFloat*(): float =
        case this.value.kind:
            of JInt:
                result = float(this.value.to(int))
            of JFloat:
                result = this.value.to(float)
            else:
                raise newException(ValueError, fmt "Cannot convert {this.value.kind} to float")

    converter toString*(): string =
        case this.value.kind:
            of JString:
                result = this.value.to(string)
            else:
                result = $this.value

    converter toBool*(): bool =
        case this.value.kind:
            of JBool:
                result = this.value.to(bool)
            else:
                result = false

    converter toSeq*(): seq[dyn] =
        case this.value.kind:
            of JArray:
                result = this.value.mapIt(it.toDyn)
            else:
                result = @[this]

    proc `$`*(): string =
        return this

    proc `%`*(): JsonNode =
        return this

    #
    # Equality
    #

    proc `==`*(that: dyn): self =
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

    proc `+`*(that: dyn): self =
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

    proc `+`*(that: auto): self {. infix } =
        result = this + self(value: % that)

    #
    # Subtraction
    #

    proc `-`*(that: dyn): self =
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

    proc `-`*(that: auto): self {. infix } =
        result = this - self(value: % that)

    #
    # Multiplication
    #

    proc `*`*(that: dyn): self =
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

    proc `*`*(that: auto): self {. infix } =
        result = this * self(value: % that)

    #
    # Multiplication
    #

    proc `/`*(that: dyn): self =
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

    proc `/`*(that: auto): self {. infix } =
        result = this / self(value: % that)

    #
    # Access
    #

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