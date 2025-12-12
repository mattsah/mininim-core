import
    mininim,
    mininim/dynamic

export
    dynamic

type
    msTK = enum
        msPunc, msWord, msString, msFloat, msBool, msInt, msOp, msEOF

    msToken = object
        case kind: msTK
            of msEOF:
                discard
            of msInt:
                intVal: int
            of msFloat:
                floatVal: float
            of msString:
                strVal: string
            of msBool:
                boolVal: bool
            of msPunc:
                puncVal: string
            of msWord:
                wordVal: string
            of msOp:
                opVal: string

    msNK = enum
        msNInt, msNFloat, msNString, msNBool, msNIdent, msNField, msNCall,
        msNBinaryOp, msNUnaryOp, msNArray, msNKey, msNObject, msNPair

    msNode = ref object
        case kind: msNK
            of msNInt:
                intVal: int
            of msNFloat:
                floatVal: float
            of msNString:
                strVal: string
            of msNBool:
                boolVal: bool
            of msNIdent:
                identName: string
            of msNField:
                fieldObj: msNode
                fieldName: string
            of msNCall:
                methodObj: msNode
                methodName: string
                methodArgs: seq[msNode]
            of msNBinaryOp:
                left: msNode
                right: msNode
                binaryOp: string
            of msNUnaryOp:
                operand: msNode
                unaryOp: string
            of msNArray:
                elements: seq[msNode]
            of msNKey:
                keyObj: msNode
                keyExpr: msNode
            of msNObject:
                pairs: seq[msNode]
            of msNPair:
                keyName: msNode
                pairVal: msNode

    Script* = ref object of Class
        tokens: seq[msToken]
        pos: int

begin msToken:
    method value(): string {. base .} =
        result = case this.kind:
            of msEOF:
                ""
            of msInt:
                $this.intVal
            of msFloat:
                $this.floatVal
            of msString:
                $this.strVal
            of msBool:
                $this.boolVal
            of msPunc:
                $this.puncVal
            of msWord:
                $this.wordVal
            of msOp:
                $this.opVal

begin msNode:
    method value(scope: dyn): dyn {. base .} =
        case this.kind:
            of msNInt:
                result = this.intVal
            of msNFloat:
                result = this.floatVal
            of msNString:
                result = this.strVal
            of msNBool:
                result = this.boolVal
            of msNIdent:
                if scope.has(this.identName):
                    result = scope[this.identName]
                else:
                    result = null
            of msNBinaryOp:
                case this.binaryOp:
                    of "+":
                        result = this.left.value(scope) + this.right.value(scope)
                    of "-":
                        result = this.left.value(scope) - this.right.value(scope)
                    of "*":
                        result = this.left.value(scope) * this.right.value(scope)
                    of "/":
                        result = this.left.value(scope) / this.right.value(scope)
                    of "==":
                        result = this.left.value(scope) == this.right.value(scope)
                    of "!=":
                        result = this.left.value(scope) != this.right.value(scope)
                    of ">=":
                        result = this.left.value(scope) >= this.right.value(scope)
                    of "<=":
                        result = this.left.value(scope) <= this.right.value(scope)
                    of ">":
                        result = this.left.value(scope) > this.right.value(scope)
                    of "<":
                        result = this.left.value(scope) < this.right.value(scope)
                    of "?":
                        if this.left.value(scope) == null:
                            result = this.right.value(scope)
                        else:
                            result = this.left.value(scope)
            of msNUnaryOp:
                case this.unaryOp:
                    of "-":
                        result = this.operand.value(scope) * -1
                    of "!":
                        result = not this.operand.value(scope)
                    else:
                         raise newException(ValueError, "Unsupported unary operator: " & this.unaryOp)
            of msNCall:
                if dyn.hasFunction(this.methodName):
                    result = dyn.callFunction(
                        this.methodName,
                        this.methodObj.value(scope),
                        this.methodArgs.mapIt(it.value(scope))
                    )

                else:
                    raise newException(ValueError, "Invalid method")
            of msNArray:
                result = ~[]
                for element in this.elements:
                    result << element.value(scope)
            of msNObject:
                result = ~()
                for pair in this.pairs:
                    result[pair.keyName.identName] = pair.pairVal.value(scope)
            of msNField:
                if this.fieldObj.value(scope).has(this.fieldName):
                    result = this.fieldObj.value(scope)[this.fieldName]
                else:
                    result = null
            of msNKey:
                result = this.keyObj.value(scope)[this.keyExpr.value(scope)]
            else:
                raise newException(ValueError, "Not implemented yet")

begin Script:
    method parseExpr(): msNode {. base .}
    method parseAccess(): msNode {. base .}

    proc lex*(code: string): seq[msToken] {. static .} =
        var
            i = 0

        while i < code.len:
            case code[i]:
                #[
                    Punc
                ]#
                of '.', '(', ')', '[', ']', ',', ':':
                    result.add(msToken(kind: msPunc, puncVal: $code[i]))

                #[
                    Symbolic Operators
                ]#
                of '+', '-', '*', '/', '?':
                    result.add(msToken(kind: msOp, opVal: $code[i]))

                of '=', '!', '>', '<':
                    if i+1 < code.len and code[i+1] in {'='}:
                        result.add(msToken(kind: msOp, opVal: $code[i..i+1]))
                        inc i
                    else:
                        result.add(msToken(kind: msOp, opVal: $code[i]))

                #[
                    Numbers
                ]#
                of '0'..'9':
                    var
                        number = ""
                        decimal = false

                    while i < code.len and code[i] in {'0'..'9', '.'}:
                        if code[i] == '.':
                            if decimal:
                                raise newException(ValueError, "Invalid syntax")
                            else:
                                decimal = true

                        number.add(code[i])

                        inc i

                    if decimal:
                        result.add(msToken(kind: msFloat, floatVal: parseFloat(number)))
                    else:
                        result.add(msToken(kind: msInt, intVal: parseInt(number)))

                    dec i

                #[
                    Idents, Keywords, and Word Operators
                ]#
                of 'a'..'z', 'A'..'Z', '_':
                    var
                        ident = ""

                    while i < code.len and code[i] in {'a'..'z', 'A'..'Z', '0'..'9', '_'}:
                        ident.add(code[i])
                        inc i

                    dec i

                    case ident.toLower:
                        of "true":
                            result.add(msToken(kind: msBool, boolVal: true))
                        of "false":
                            result.add(msToken(kind: msBool, boolVal: false))
                        of "not":
                            result.add(msToken(kind: msOp, opVal: $'!'))
                        of "and":
                            result.add(msToken(kind: msOp, opVal: $'&'))
                        of "or":
                            result.add(msToken(kind: msOp, opVal: $'|'))
                        else:
                            result.add(msToken(kind: msWord, wordVal: ident))

                #[
                    Strings
                ]#
                of '\'', '"':
                    var
                        close = code[i]
                        value = ""

                    inc i
                    while i < code.len:
                        if code[i] == '\\':
                            inc i
                            case code[i]:
                                of 'n': value.add('\n')
                                of 't': value.add('\t')
                                of '\\': value.add('\\')
                                else:
                                    if code[i+1] == close:
                                        value.add(close)
                                    else:
                                        discard
                        elif code[i] != close:
                            value.add(code[i])
                            inc i
                        else:
                            break

                    result.add(msToken(kind: msString, strVal: value))

                else:
                    discard
            inc i

    proc eval*(code: string, scope: dyn = ()): dyn {. static .} =
        let
            tokens = self.lex(code)
            parser = self(tokens: tokens)

        let
            tree = parser.parseExpr()

        result = tree.value(scope)

    proc fill*(format: string, scope: dyn = ()): dyn {. static .} =
        var
            head = 0
            index = 1
            inBlock = false
            working = format

        while index < working.len:
            if not inBlock and working[index - 1..index] == "{{":
                inBlock = true
                head = index + 1
            if inBlock and working[index - 1..index] == "}}":
                let
                    code = working[head..index - 2]
                    value = Script.eval(code, scope)

                inBlock = false
                working = working[0..head - 3] & value & working[index + 1..<working.len]
                index = head - 3 + asString(value).len

            inc index

        result = working

    method current(): msToken {. base .} =
        if this.pos < this.tokens.len:
            result = this.tokens[this.pos]
        else:
            result = msToken(kind: msEOF)

    method consume(expected: msTK, value: string = "") {. base .} =
        if this.current.kind == expected and (value == "" or value == this.current.value):
            inc this.pos
        else:
            raise newException(
                ValueError,
                fmt "Expected {$expected}[{$value}] but got {$this.current.kind}[{$this.current.value}]"
            )

    method parsePrimary(): msNode {. base .} =
        case this.current.kind
            of msInt:
                result = msNode(
                    kind: msNInt,
                    intVal: this.current.intVal
                )
                this.consume(msInt)

            of msFloat:
                result = msNode(
                    kind: msNFloat,
                    floatVal: this.current.floatVal
                )
                this.consume(msFloat)

            of msString:
                result = msNode(
                    kind: msNString,
                    strVal: this.current.strVal
                )
                this.consume(msString)

            of msBool:
                result = msNode(
                    kind: msNBool,
                    boolVal: this.current.boolVal
                )
                this.consume(msBool)

            of msWord:
                let
                    name = this.current.wordVal

                result = msNode(
                    kind: msNIdent,
                    identName: name
                )
                this.consume(msWord)

                if this.current.kind == msPunc and this.current.value == ":":
                    this.consume(msPunc, ":")
                    var
                        value = this.parseExpr()

                    result = msNode(
                        kind: msNPair,
                        keyName: result,
                        pairVal: value
                    )

            of msPunc:  # Array literal: [1, 2, 3]
                if this.current.value == "[":
                    this.consume(msPunc, "[")
                    var
                        elements: seq[msNode]

                    # Parse array elements
                    if this.current.kind != msPunc or this.current.value != "]":
                        while true:
                            var
                                value = this.parseExpr()

                            elements.add(value)

                            if this.current.kind == msPunc and this.current.value != ",":
                                break

                            this.consume(msPunc, ",")

                    this.consume(msPunc, "]")

                    result = msNode(
                        kind: msNArray,
                        elements: elements
                    )

                elif this.current.value == "(":
                    this.consume(msPunc, "(")

                    result = this.parseExpr()

                    if result.kind == msNPair:
                        var
                            pairs: seq[msNode] = @[result]

                        while true:
                            if this.current.kind == msPunc:
                                if this.current.value == ")":
                                    break
                                elif this.current.value == ",":
                                    this.consume(msPunc, ",")

                                    let
                                        pair = this.parseExpr()

                                    if pair.kind == msNPair:
                                        pairs.add(pair)
                                        continue

                            raise newException(
                                ValueError,
                                "Shitty Colon"
                            )

                        result = msNode(
                            kind: msNObject,
                            pairs: pairs
                        )

                    this.consume(msPunc, ")")

            of msOp: # Unary
                if this.current.value == "-":
                    this.consume(msOp, "-")
                    result = msNode(
                        kind: msNUnaryOp,
                        unaryOp: "-",
                        operand: this.parseAccess()
                    )
                elif this.current.value == "!":
                    this.consume(msOp, "!")
                    result = msNode(
                        kind: msNUnaryOp,
                        unaryOp: "!",
                        operand: this.parseAccess()
                    )
            else:
                raise newException(
                    ValueError,
                    "Unexpected token in primary: " & $this.current.kind
                )

    # Parse field access and method calls: foo.bar.test("1", bar.foo)
    method parseAccess(): msNode {. base .} =
        result = this.parsePrimary()

        while this.current.kind == msPunc and this.current.value[0] in {'.', '(', '['}:
            if this.current.value == "[":
                this.consume(msPunc, "[")

                let
                    key = this.parseExpr()

                this.consume(msPunc, "]")

                result = msNode(
                    kind: msNKey,
                    keyObj: result,
                    keyExpr: key
                )

            elif this.current.value == ".":
                this.consume(msPunc, ".")

                if this.current.kind == msWord:
                    let
                        field = this.current.value

                    this.consume(msWord)

                    # Check if this is followed by parentheses (method call)
                    if this.current.kind == msPunc and this.current.value[0] == '(':
                        this.consume(msPunc, "(")

                        var
                            args: seq[msNode]

                        # Parse arguments
                        if this.current.kind != msPunc or this.current.value[0] != ')':
                            while true:
                                args.add(this.parseExpr())

                                if this.current.kind != msPunc or this.current.value[0] != ',':
                                    break

                                this.consume(msPunc, ",")

                        this.consume(msPunc, ")")

                        result = msNode(
                            kind: msNCall,
                            methodObj: result,
                            methodName: field,
                            methodArgs: args
                        )

                    # Regular field access
                    else:
                        result = msNode(
                            kind: msNField,
                            fieldObj: result,
                            fieldName: field
                        )

    # Parse multiplication/division
    method parseTerm(): msNode {. base .} =
        result = this.parseAccess()

        while this.current.kind == msOp and this.current.value in ["*", "/"]:
            let
                operator = this.current.value

            this.consume(this.current.kind)

            result = msNode(
                kind: msNBinaryOp,
                left: result,
                right: this.parseAccess(),
                binaryOp: operator
            )

    # Parse addition/subtraction
    method parseExpr(): msNode {. base .}=
        result = this.parseTerm()

        while this.current.kind == msOp and this.current.value in ["+", "-", "&", "|", "?", "==", "!=", ">", "<", ">=", "<="]:
            let
                operator = this.current.value

            this.consume(this.current.kind)

            result = msNode(
                kind: msNBinaryOp,
                left: result,
                right: this.parseTerm(),
                binaryOp: operator
            )
