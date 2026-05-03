module Drip.Eval
open Drip
open System.Globalization

let rec eval (env: Map<string, Value>) expr =
    match expr with
    | Constant(v) -> 
        match v with
        | :? int as i -> VInt i
        | :? float as f -> VFloat f
        | :? bool as b -> VBool b
        | :? string as s -> VStr s
        | _ -> failwith "Unknown constant"
    
    | Variable(name) -> 
        match env.TryFind name with
        | Some v -> v
        | None -> failwithf "Bean not found: %s" name

    | Sip -> 
        let input = System.Console.ReadLine()
        VStr(if input = null then "" else input)

    | Cast(targetType, e) ->
        let v = eval env e
        match targetType, v with
        | "int", VStr s -> 
            match System.Int32.TryParse(s) with
            | true, i -> VInt i
            | _ -> failwithf "Cannot grind string '%s' to int" s
        | "int", VFloat f -> VInt(int f)
        | "int", VInt i -> VInt i
        | "float", VStr s ->
            let success, f = System.Double.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture)
            if success then VFloat f else failwithf "Cannot grind string '%s' to float" s
        | "float", VInt i -> VFloat(float i)
        | "float", VFloat f -> VFloat f
        | "str", VInt i -> VStr(string i)
        | "str", VFloat f -> VStr(f.ToString(CultureInfo.InvariantCulture))
        | "str", VBool b -> VStr(if b then "Coffee" else "Tea")
        | "str", VStr s -> VStr s
        | "bool", VStr s -> VBool(s.ToLower() = "coffee" || s.ToLower() = "true")
        | "bool", VBool b -> VBool b
        | _, _ -> failwithf "Cannot grind %A to %s" v targetType

    | Sequence(e1, e2) ->
        eval env e1 |> ignore 
        eval env e2           

    | Print(e) ->
        let v = eval env e
        match v with
        | VInt i -> printfn "%d" i
        | VFloat f -> printfn "%g" f
        | VBool b -> printfn "%s" (if b then "Coffee" else "Tea")
        | VStr s -> printfn "%s" s 
        | _ -> printfn "%A" v
        v

    | Assignment(name, valueExpr, body) ->
        let v = eval env valueExpr
        let finalV = 
            match v with
            | VClosure(ps, b, cEnv) -> VRecClosure(name, ps, b, cEnv)
            | _ -> v
        eval (env.Add(name, finalV)) body

    | BinaryOp(op, l, r) ->
        let lv = eval env l
        let rv = eval env r
        match lv, rv with
        | VInt a, VInt b ->
            match op with
            | "+" -> VInt(a + b) | "-" -> VInt(a - b)
            | "*" -> VInt(a * b) | "/" -> if b = 0 then failwith "Zero division" else VInt(a / b)
            | "==" -> VBool(a = b) | _ -> failwith "Unknown op"
        | VFloat a, VFloat b ->
            match op with
            | "+" -> VFloat(a + b) | "-" -> VFloat(a - b)
            | "*" -> VFloat(a * b) | "/" -> VFloat(a / b)
            | "==" -> VBool(a = b) | _ -> failwith "Unknown op"
        | VInt a, VFloat b ->
            match op with
            | "+" -> VFloat(float a + b) | "-" -> VFloat(float a - b)
            | "*" -> VFloat(float a * b) | "/" -> VFloat(float a / b)
            | "==" -> VBool(float a = b) | _ -> failwith "Unknown op"
        | VFloat a, VInt b ->
            match op with
            | "+" -> VFloat(a + float b) | "-" -> VFloat(a - float b)
            | "*" -> VFloat(a * float b) | "/" -> VFloat(a / float b)
            | "==" -> VBool(a = float b) | _ -> failwith "Unknown op"
        | VStr a, VStr b when op = "+" -> VStr(a + b)
        | VStr a, VInt b when op = "+" -> VStr(a + string b)
        | VStr a, VFloat b when op = "+" -> VStr(a + b.ToString(CultureInfo.InvariantCulture))
        | _ -> failwith "Type mismatch"

    | If(cond, e1, e2) ->
        match eval env cond with
        | VBool true -> eval env e1
        | VBool false -> eval env e2
        | _ -> failwith "Aroma check failed"

    | Function(ps, b) -> VClosure(ps, b, env)

    | Call(fExpr, argExprs) ->
        let fVal = eval env fExpr
        let argVals = argExprs |> List.map (eval env)
        
        let apply ps b cEnv =
            if List.length ps <> List.length argVals then
                failwithf "Expected %d beans, but got %d" (List.length ps) (List.length argVals)
            let newEnv = List.fold2 (fun acc p v -> Map.add p v acc) cEnv ps argVals
            eval newEnv b

        match fVal with
        | VClosure(ps, b, cEnv) -> apply ps b cEnv
        | VRecClosure(fName, ps, b, cEnv) -> apply ps b (Map.add fName fVal cEnv)
        | _ -> failwith "Not a function"