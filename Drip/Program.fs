open FParsec
open Drip.Parser
open Drip.Eval
open System.IO // Для работы с файлами

[<EntryPoint>]
let main argv =
    // Проверяем, передан ли путь к файлу
    if argv.Length = 0 then
        printfn "Usage: drip <filename.drip>"
        1
    else
        let filePath = argv.[0]
        if not (File.Exists(filePath)) then
            printfn "Error: File %s not found." filePath
            1
        else
            let code = File.ReadAllText(filePath)
            
            // Запускаем парсер
            match run (spaces >>. pExpr .>> eof) code with
            | Success(result, _, _) ->
                try
                    eval Map.empty result |> ignore
                    0
                with
                | ex -> 
                    printfn "Runtime Error: %s" ex.Message
                    1
            | Failure(err, _, _) -> 
                printfn "Parser Error:\n%s" err
                1