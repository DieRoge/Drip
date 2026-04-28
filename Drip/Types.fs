namespace Drip

// Все типы данных нашего языка
type Expr =
    | Constant of obj
    | Variable of string
    | BinaryOp of string * Expr * Expr
    | Assignment of string * Expr * Expr
    | Function of string * Expr
    | Call of Expr * Expr
    | If of Expr * Expr * Expr
    | Print of Expr

type Value =
    | VInt of int
    | VBool of bool
    | VClosure of string * Expr * Map<string, Value>
    // НОВОЕ: Рекурсивное замыкание (имя_функции, параметр, тело, окружение)
    | VRecClosure of string * string * Expr * Map<string, Value>