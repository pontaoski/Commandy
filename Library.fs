namespace Commandy

type Expression =
    | ExprCommandApplication of CommandApplication
    | ExprPipeline of Expression * Expression
    | ExprString of string
    | ExprNum of int32
and Argument =
    | Arg of string
    | ArgExpr of Expression
and CommandApplication = string * (Argument list)

module Parser =
    open FParsec
    open FParsec.Primitives
    open FParsec.CharParsers

    let opp: OperatorPrecedenceParser<Expression, Position, obj> = new OperatorPrecedenceParser<Expression, _, _>()

    let identifierLiteral: Parser<string, _> =
        let firstChar c = isLetter c
        let generalChar c = firstChar c || isDigit c || c = '-'

        many1Satisfy2L firstChar generalChar "identifier"

    let argTermParser =
        let mostThingsLiteral: Parser<string, _> =
            let notEnder (c: char) = c <> ' ' && c <> ')' && c <> '|' && c <> '@'
            many1Satisfy2L notEnder notEnder "anything but space"

        let plainParser = mostThingsLiteral .>> spaces |>> Arg
        let quotedParser = between (pstring "'") (pstring "'") (manyChars (noneOf "'")) |>> Arg
        let exprParser = pchar '(' >>. opp.ExpressionParser  .>> pchar ')' |>> ArgExpr

        exprParser <|> quotedParser <|> plainParser

    let argParser =
        many argTermParser

    let commandApplicationParser =
        identifierLiteral .>> spaces .>>. argParser

    let termParser =
        let numberParser = pint32 .>> spaces |>> ExprNum
        let cmdAppl = commandApplicationParser |>> fun (ident, args) -> ExprCommandApplication(ident, args)
        numberParser <|> cmdAppl

    opp.TermParser <- termParser

    let adjustPosition offset (pos: Position) =
        Position(pos.StreamName, pos.Index + int64 offset,
                pos.Line, pos.Column + int64 offset)

    // To simplify infix operator definitions, we define a helper function.
    let addInfixOperator str prec assoc mapping =
        let op = InfixOperator(str, getPosition .>> spaces, prec, assoc, (),
            fun opPos leftTerm rightTerm ->
                mapping
                    (adjustPosition -str.Length opPos)
                    leftTerm rightTerm)
        opp.AddOperator(op)

    addInfixOperator
        "|" 1 Associativity.Left
        (fun opPos leftTerm rightTerm ->
            (ExprPipeline (leftTerm, rightTerm)))

    addInfixOperator
        "@" 2 Associativity.Right
        (fun opPos leftTerm rightTerm ->
            (ExprPipeline (rightTerm, leftTerm)))

    let parseInput (input: string) =
        let reply: Reply<Expression> = opp.ExpressionParser (new CharStream<obj>(input, 0, input.Length))
        reply.Result
