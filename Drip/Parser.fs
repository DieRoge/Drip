module Drip.Parser

open FParsec
open Drip

// 1. Умная обработка пробелов и комментариев
let pComment = pstring "#" >>. skipRestOfLine true
let spaces = skipMany (spaces1 <|> pComment)

let str s = pstring s
let token s = pstring s .>> spaces

let pExpr, pExprRef = createParserForwardedToRef<Expr, unit>()

// 2. Новые парсеры для строк и ввода
let pSip = token "sip" >>% Sip
let pStringLiteral = 
    between (str "\"") (str "\"") (manyChars (noneOf "\"")) 
    .>> spaces 
    |>> (fun s -> Constant(s))

let pNumber = pint32 .>> spaces |>> (fun n -> Constant(n))
let pCoffee = token "Coffee" >>% Constant(true)
let pTea = token "Tea" >>% Constant(false)
let pIdentifierRaw = identifier (IdentifierOptions()) .>> spaces
let pVariable = pIdentifierRaw |>> Variable

let pCall =
    pIdentifierRaw .>> spaces .>>. choice [
        between (token "(") (token ")") pExpr
        pNumber 
        pStringLiteral // Теперь функцию можно вызвать со строкой [cite: 35]
        pVariable
    ] |>> (fun (f, a) -> Call(Variable f, a))

let pTerm = choice [ 
    attempt pCall
    between (token "(") (token ")") pExpr
    pNumber 
    pStringLiteral // Строка как терм в выражении [cite: 37]
    pCoffee 
    pTea 
    pVariable 
]
// Настройка OperatorPrecedenceParser остается прежней [cite: 38-43]
let opp = OperatorPrecedenceParser<Expr, unit, unit>()
opp.TermParser <- pTerm
opp.AddOperator(InfixOperator("==", spaces, 1, Associativity.Left, fun x y -> BinaryOp("==", x, y)))
opp.AddOperator(InfixOperator("+", spaces, 2, Associativity.Left, fun x y -> BinaryOp("+", x, y)))
opp.AddOperator(InfixOperator("-", spaces, 2, Associativity.Left, fun x y -> BinaryOp("-", x, y)))
opp.AddOperator(InfixOperator("*", spaces, 3, Associativity.Left, fun x y -> BinaryOp("*", x, y)))

let pIf = pipe3 (token "aroma" >>. pExpr) (token "hot" >>. pExpr) (token "iced" >>. pExpr) (fun c e1 e2 -> If(c, e1, e2))
let pFunc = token "order" >>. token "brew" >>. pIdentifierRaw .>> token "~>" .>>. pExpr |>> (fun (p, b) -> Function(p, b))
let pBrew = token "brew" >>. token "order" >>. pIdentifierRaw .>> token "|>" .>>. pExpr .>> token "serve" .>>. pExpr |>> (fun ((n, v), b) -> Assignment(n, v, b))
let pPour = token "pour" >>. pExpr |>> Print

pExprRef.Value <- spaces >>. choice [
    attempt pBrew   
    attempt pIf     
    attempt pFunc   
    attempt pPour   
    attempt pSip // Добавляем sip в общий выбор [cite: 43]
    opp.ExpressionParser 
]