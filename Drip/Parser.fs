module Drip.Parser

open FParsec
open Drip

// Комментарии и пробелы
let pComment = pstring "#" >>. skipRestOfLine true
let spaces = skipMany (spaces1 <|> pComment)
let token s = pstring s .>> spaces

let pExpr, pExprRef = createParserForwardedToRef<Expr, unit>()

// Атомы
let pSip = token "sip" >>% Sip
let pString = between (pstring "\"") (pstring "\"") (manyChars (noneOf "\"")) .>> spaces |>> (fun s -> Constant(s))
let pNumber = pint32 .>> spaces |>> (fun n -> Constant(n))
let pIdentifier = identifier (IdentifierOptions()) .>> spaces

let pTerm = choice [
    attempt (pIdentifier .>> spaces .>>. choice [ between (token "(") (token ")") pExpr; pNumber; pString; (pIdentifier |>> Variable) ] |>> (fun (f, a) -> Call(Variable f, a)))
    between (token "(") (token ")") pExpr
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

// Упрощенные команды
let pIf = pipe3 (token "aroma" >>. pExpr) (token "hot" >>. pExpr) (token "iced" >>. pExpr) (fun c e1 e2 -> If(c, e1, e2))
let pFunc = token "order" >>. pIdentifier .>> token "~>" .>>. pExpr |>> (fun (p, b) -> Function(p, b))
let pBrew = token "brew" >>. pIdentifier .>> token "|>" .>>. pExpr .>> token "serve" .>>. pExpr |>> (fun ((n, v), b) -> Assignment(n, v, b))
let pPour = token "pour" >>. pExpr |>> Print

pExprRef.Value <- spaces >>. choice [
    attempt pBrew   
    attempt pIf     
    attempt pFunc   
    attempt pPour   
    attempt pSip
    opp.ExpressionParser 
]