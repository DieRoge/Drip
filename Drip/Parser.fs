module Drip.Parser

open FParsec
open Drip

let str s = pstring s
let token s = pstring s .>> spaces

let pExpr, pExprRef = createParserForwardedToRef<Expr, unit>()

// 1. Атомы (только значения)
let pNumber = pint32 .>> spaces |>> (fun n -> Constant(n))
let pCoffee = token "Coffee" >>% Constant(true)
let pTea = token "Tea" >>% Constant(false)
let pIdentifierRaw = identifier (IdentifierOptions()) .>> spaces
let pVariable = pIdentifierRaw |>> Variable

// НОВОЕ: Парсер для вызова функции: Имя + Пробел + Аргумент
let pCall =
    pIdentifierRaw .>> spaces .>>. choice [
        between (token "(") (token ")") pExpr // Аргумент в скобках: fact (n - 1)
        pNumber                               // Или просто число: fact 5
        pCoffee
        pTea
        pVariable
    ]
    |>> (fun (fName, arg) -> Call(Variable fName, arg))

// ОБНОВЛЕННОЕ: Добавляем attempt pCall на первое место
let opp = OperatorPrecedenceParser<Expr, unit, unit>()
let pTerm = choice [ 
    attempt pCall // ОЧЕНЬ ВАЖНО: сначала пробуем вызов функции!
    between (token "(") (token ")") pExpr
    pNumber 
    pCoffee 
    pTea 
    pVariable 
]
opp.TermParser <- pTerm

opp.AddOperator(InfixOperator("==", spaces, 1, Associativity.Left, fun x y -> BinaryOp("==", x, y)))
opp.AddOperator(InfixOperator("+", spaces, 2, Associativity.Left, fun x y -> BinaryOp("+", x, y)))
opp.AddOperator(InfixOperator("-", spaces, 2, Associativity.Left, fun x y -> BinaryOp("-", x, y)))
opp.AddOperator(InfixOperator("*", spaces, 3, Associativity.Left, fun x y -> BinaryOp("*", x, y)))

// 3. Конструкции (используют opp.ExpressionParser внутри себя, если нужно)
let pIf = 
    pipe3 (token "aroma" >>. pExpr)
          (token "hot" >>. pExpr)
          (token "iced" >>. pExpr)
          (fun cond e1 e2 -> If(cond, e1, e2))

let pFunc =
    token "order" >>. token "brew" >>. 
    pIdentifierRaw .>> token "~>" .>>. pExpr
    |>> (fun (p, b) -> Function(p, b))

let pBrew =
    token "brew" >>. token "order" >>. 
    pIdentifierRaw .>> token "|>" .>>. pExpr 
    .>> token "serve" .>>. pExpr
    |>> (fun ((n, v), b) -> Assignment(n, v, b))

let pPour = token "pour" >>. pExpr |>> Print

// 4. ГЛАВНАЯ ТОЧКА ВХОДА
// Теперь мы СНАЧАЛА проверяем ключевые слова, и только потом — всё остальное
pExprRef.Value <- spaces >>. choice [
    attempt pBrew   
    attempt pIf     
    attempt pFunc   
    attempt pPour   
    opp.ExpressionParser 
]