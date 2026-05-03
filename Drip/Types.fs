namespace Drip

type Expr =
    | Constant of obj
    | Variable of string
    | BinaryOp of string * Expr * Expr
    | Assignment of string * Expr * Expr
    | Function of string list * Expr  // Список имен аргументов
    | Call of Expr * Expr list        // Список выражений-аргументов
    | If of Expr * Expr * Expr
    | Print of Expr
    | Sequence of Expr * Expr
    | Sip
    | Cast of string * Expr 

type Value =
    | VInt of int
    | VFloat of float       
    | VBool of bool
    | VStr of string
    | VClosure of string list * Expr * Map<string, Value>
    | VRecClosure of string * string list * Expr * Map<string, Value>