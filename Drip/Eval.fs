module Drip.Eval
open Drip

let rec eval (env: Map<string, Value>) expr =
    match expr with
    | Constant(v) -> 
        match v with
        | :? int as i -> VInt i
        | :? bool as b -> VBool b
        | :? string as s -> VStr s
        | _ -> failwith "Unknown constant"
    
    | Variable(name) -> 
        match env.TryFind name with
        | Some v -> v
        | None -> failwithf "Bean not found: %s" name

    | Sip -> VStr(System.Console.ReadLine())

    | Print(e) ->
        let v = eval env e
        match v with
        | VInt i -> printfn "=> %d" i
        | VBool b -> printfn "=> %b" b
        | VStr s -> printfn "=> %s" s
        | _ -> printfn "=> %A" v
        v

    | If(cond, e1, e2) ->
        match eval env cond with
        | VBool true -> eval env e1
        | VBool false -> eval env e2
        | _ -> failwith "Aroma check failed"

    | Assignment(name, valueExpr, body) ->
        let v = 
            match valueExpr with
            | Function(param, fBody) -> VRecClosure(name, param, fBody, env)
            | _ -> eval env valueExpr
        eval (env.Add(name, v)) body

    | Function(param, body) -> VClosure(param, body, env)

    | Call(fExpr, argExpr) ->
        let fVal = eval env fExpr
        let argVal = eval env argExpr
        match fVal with
        | VClosure(p, b, cEnv) -> eval (cEnv.Add(p, argVal)) b
        | VRecClosure(fName, p, b, cEnv) -> 
            eval (cEnv.Add(fName, fVal).Add(p, argVal)) b
        | _ -> failwith "Not a function"

    | BinaryOp(op, l, r) ->
        match eval env l, eval env r with
        | VInt a, VInt b ->
            match op with
            | "+" -> VInt(a + b) | "-" -> VInt(a - b)
            | "*" -> VInt(a * b) | "==" -> VBool(a = b)
            | _ -> failwithf "Unknown operator: %s" op
        | VStr a, VStr b when op = "+" -> VStr(a + b)
        | _ -> failwith "Type mismatch"