namespace Drip

type Expr =
    | Constant of obj
    | Variable of string
    | BinaryOp of string * Expr * Expr
    | Assignment of string * Expr * Expr
    | Function of string * Expr
    | Call of Expr * Expr
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
    | VClosure of string * Expr * Map<string, Value>
    | VRecClosure of string * string * Expr * Map<string, Value>