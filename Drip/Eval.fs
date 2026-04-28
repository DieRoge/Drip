module Drip.Eval

open Drip

let rec eval (env: Map<string, Value>) expr =
    match expr with
    | Constant(v) -> 
        match v with
        | :? int as i -> VInt i
        | :? bool as b -> VBool b
        | _ -> failwith "Unknown constant"
    
    | Variable(name) -> 
        match env.TryFind name with
        | Some v -> v
        | None -> failwithf "Bean not found: %s" name

    | Print(e) ->
        let v = eval env e
        match v with
        | VInt i -> printfn "=> %d" i
        | VBool b -> printfn "=> %b" b
        | _ -> printfn "=> %A" v
        v

    | If(cond, e1, e2) ->
        match eval env cond with
        | VBool true -> eval env e1
        | VBool false -> eval env e2
        | _ -> failwith "Aroma check failed: not a boolean"

    | Assignment(name, valueExpr, body) ->
        let v = 
            match valueExpr with
            | Function(param, fBody) -> 
                // Если мы присваиваем функцию переменной, мы знаем её имя! 
                // Создаем рекурсивное замыкание:
                VRecClosure(name, param, fBody, env)
            | _ -> eval env valueExpr
        eval (env.Add(name, v)) body

    | Function(param, body) ->
        // Анонимные функции остаются обычными замыканиями
        VClosure(param, body, env)

    | Call(fExpr, argExpr) ->
        let fVal = eval env fExpr
        let argVal = eval env argExpr
        match fVal with
        | VClosure(param, body, cEnv) -> 
            eval (cEnv.Add(param, argVal)) body
        | VRecClosure(fName, param, body, cEnv) -> 
            // МАГИЯ РЕКУРСИИ: Перед выполнением тела добавляем саму функцию в её же окружение
            let envWithSelf = cEnv.Add(fName, fVal)
            eval (envWithSelf.Add(param, argVal)) body
        | _ -> failwith "This isn't a blend (not a function)"

    | BinaryOp(op, l, r) ->
        match eval env l, eval env r with
        | VInt a, VInt b ->
            match op with
            | "+" -> VInt(a + b)
            | "-" -> VInt(a - b)
            | "*" -> VInt(a * b)
            | "==" -> VBool(a = b)
            | _ -> failwithf "Unknown operator: %s" op
        | _ -> failwith "Math requires Espresso (integers)"