module Drip.Parser
open FParsec
open Drip

let pComment = pstring "#" >>. skipRestOfLine true
let spaces = skipMany (spaces1 <|> pComment)
let token s = pstring s .>> spaces

let keywords = [
    "brew"; "pour"; "aroma"; "hot"; "iced"; 
    "order"; "sip"; "Coffee"; "Tea"; "grind"; "int"; "str"; "bool"; "float"
]

// Исправленный pIdentifier: не потребляет ввод, если это ключевое слово
let pIdentifier = 
    (lookAhead (identifier (IdentifierOptions())) >>= fun s -> 
        if List.contains s keywords then fail "Keyword cannot be used as an identifier"
        else identifier (IdentifierOptions())) .>> spaces

let pType = choice [ token "int"; token "str"; token "bool"; token "float" ]

let pExprRef = ref (fun (col: int64) -> fail "uninitialized" : Parser<Expr, unit>)
let pExpr col : Parser<Expr, unit> = fun stream -> (!pExprRef col) stream

let pStmtRef = ref (fun (col: int64) -> fail "uninitialized" : Parser<Expr, unit>)
let pStmt col : Parser<Expr, unit> = fun stream -> (!pStmtRef col) stream

let buildAST (exprs: Expr list) =
    let rec foldRight = function
        | [] -> Constant(0)
        | [e] -> 
            match e with
            | Assignment(n, v, _) -> Assignment(n, v, Constant(0))
            | _ -> e
        | e :: rest ->
            match e with
            | Assignment(n, v, _) -> Assignment(n, v, foldRight rest)
            | _ -> Sequence(e, foldRight rest)
    foldRight exprs

let pBlockExact (col: int64) : Parser<Expr, unit> =
    many1 (
        attempt (
            spaces >>. getPosition >>= fun pos ->
                if pos.Column = col then pStmt col
                else fail "Indentation mismatch"
        )
    ) |>> buildAST

let pBlockGt (parentCol: int64) : Parser<Expr, unit> =
    spaces >>. getPosition >>= fun pos ->
        if pos.Column > parentCol then pBlockExact pos.Column
        else fail "Expected indented block"

let pSip = token "sip" >>% Sip
let pString = between (pstring "\"") (pstring "\"") (manyChars (noneOf "\"")) .>> spaces |>> (fun s -> Constant(s))

let pNumber = 
    numberLiteral NumberLiteralOptions.AllowFraction "number" .>> spaces 
    |>> fun nl -> 
        if nl.IsInteger then Constant(int32 nl.String)
        else Constant(float nl.String)

let pCall =
    pIdentifier .>>. choice [
        between (token "(") (token ")") (spaces >>. getPosition >>= fun pos -> pBlockExact pos.Column)
        pNumber
        pString
        (pIdentifier |>> Variable)
    ] |>> (fun (f, a) -> Call(Variable f, a))

let pTerm = choice [
    attempt pCall
    // ВАЖНО: используем pExpr 0L, а не pExprRef.Value
    attempt (token "grind" >>. pType .>>. pExpr 0L |>> Cast)
    between (token "(") (token ")") (spaces >>. getPosition >>= fun pos -> pBlockExact pos.Column)
    pNumber
    pString
    token "Coffee" >>% Constant(true)
    token "Tea" >>% Constant(false)
    pIdentifier |>> Variable
]

let opp = OperatorPrecedenceParser<Expr, unit, unit>()
opp.TermParser <- pTerm
opp.AddOperator(InfixOperator("==", spaces, 1, Associativity.Left, fun x y -> BinaryOp("==", x, y)))
opp.AddOperator(InfixOperator("+", spaces, 2, Associativity.Left, fun x y -> BinaryOp("+", x, y)))
opp.AddOperator(InfixOperator("-", spaces, 2, Associativity.Left, fun x y -> BinaryOp("-", x, y)))
opp.AddOperator(InfixOperator("*", spaces, 3, Associativity.Left, fun x y -> BinaryOp("*", x, y)))
opp.AddOperator(InfixOperator("/", spaces, 3, Associativity.Left, fun x y -> BinaryOp("/", x, y)))

let pIf col = 
    token "aroma" >>. pExpr col .>> token "hot" >>= fun cond ->
    pBlockGt col >>= fun e1 ->
    token "iced" >>. pBlockGt col >>= fun e2 ->
    preturn (If(cond, e1, e2))

let pFunc col = 
    token "order" >>. pIdentifier .>> token "~>" >>= fun p ->
    pBlockGt col |>> (fun b -> Function(p, b))

let pBrew col = 
    token "brew" >>. pIdentifier .>>. 
    opt (attempt (token "grind" >>. pType)) .>> token "|>" .>>. pExpr col
    |>> (fun ((n, castOpt), v) -> 
        let expr = match castOpt with | Some t -> Cast(t, v) | None -> v
        Assignment(n, expr, Constant(0)))

let pPour col = token "pour" >>. pExpr col |>> Print

pStmtRef.Value <- fun col -> choice [
    attempt (pIf col)
    attempt (pFunc col)
    attempt (pBrew col)
    attempt (pPour col)
    attempt pSip
    opp.ExpressionParser
]
pExprRef.Value <- pStmtRef.Value

let pProgram = 
    spaces >>. many (attempt (pStmt 0L)) .>> eof 
    |>> buildAST