open System.Text.RegularExpressions
open System.Collections.Generic

type LispVal =
    | Symbol of string
    | Number of int
    | List of LispVal list
    | DottedList of LispVal list * LispVal
    | String of string
    | Bool of bool
    | Func of (LispVal list -> LispVal)

type Env = Dictionary<string, LispVal>

let globalEnv: Env = Dictionary<string, LispVal>()

let rec tokenize (input: string): string list =
    let pattern = @"[\s,]*([()]|""(?:[^""]|\\.)*""|[^\s(),""]+)"
    [ for m in Regex.Matches(input, pattern) -> m.Groups.[1].Value ]

let rec readExpr tokens =
    match tokens with
    | [] -> failwith "Unexpected end of input"
    | "(" :: rest ->
        let rec parseList acc tokens =
            match tokens with
            | [] -> failwith "Unexpected end of input"
            | ")" :: rest -> (List (List.rev acc), rest)
            | _ ->
                let (expr, remainingTokens) = readExpr tokens
                parseList (expr :: acc) remainingTokens
        parseList [] rest
    | ")" :: _ -> failwith "Unexpected )"
    | token :: rest ->
        let atom =
            match token with
            | "true" -> Bool true
            | "false" -> Bool false
            | _ when Regex.IsMatch(token, @"^-?\d+$") -> Number (int token)
            | _ -> Symbol token
        (atom, rest)

let rec eval expr (env: Env) =
    match expr with
    | Symbol s ->
        match env.TryGetValue(s) with
        | true, value -> value
        | false, _ -> failwithf "Unknown symbol: %s" s
    | Number n -> Number n
    | Bool b -> Bool b
    | List (Symbol "quote" :: xs) -> List xs
    | List (Symbol "if" :: test :: conseq :: alt :: []) ->
        match eval test env with
        | Bool true -> eval conseq env
        | Bool false -> eval alt env
        | _ -> failwith "Invalid condition"
    | List (Symbol "define" :: Symbol name :: expr :: []) ->
        let value = eval expr env
        defineFunction name value env
        Symbol name
    | List (Symbol "lambda" :: List prms :: body) ->
        let paramNames = List.map (function Symbol name -> name | _ -> failwith "Invalid parameter") prms
        Func (fun args ->
            let localEnv: Env = Dictionary<string, LispVal>(env)
            List.iter2 (fun param arg -> localEnv.[param] <- arg) paramNames args
            evalBody localEnv body) 
    | List (funcExpr :: args) ->
        let funcVal = eval funcExpr env
        apply funcVal (List.map (fun arg -> eval arg env) args)
    | _ -> expr

and evalBody env body =
    match body with
    | [] -> failwith "No body in lambda"
    | exprs ->
        exprs |> List.map (fun expr -> eval expr env) |> List.last 

and apply func args =
    match func with
    | Func f -> f args
    | _ -> failwithf "Unknown function: %A" func

and defineFunction name func (env: Env) =
    env.[name] <- func 
    printfn "Defined function %s" name

// Define built-in functions with support for multiple arguments
let builtInFunctions = [
    "+", Func (fun args ->
        let rec sum acc lst =
            match lst with
            | [] -> Number acc
            | Number n :: rest -> sum (acc + n) rest
            | _ -> failwith "Invalid arguments for +"
        sum 0 args)
    "-", Func (fun args ->
        match args with
        | [] -> failwith "Invalid arguments for -"
        | [Number a] -> Number (-a)
        | Number a :: rest ->
            let rec subtract acc lst =
                match lst with
                | [] -> Number acc
                | Number n :: rest -> subtract (acc - n) rest
                | _ -> failwith "Invalid arguments for -"
            subtract a rest
        | _ -> failwith "Invalid arguments for -")
    "*", Func (fun args ->
        let rec prod acc lst =
            match lst with
            | [] -> Number acc
            | Number n :: rest -> prod (acc * n) rest
            | _ -> failwith "Invalid arguments for *"
        prod 1 args)
    "/", Func (fun args ->
        match args with
        | [] -> failwith "Invalid arguments for /"
        | [Number a] -> Number (1 / a)
        | Number a :: rest ->
            let rec divide acc lst =
                match lst with
                | [] -> Number acc
                | Number 0 :: _ -> failwith "Division by zero"
                | Number n :: rest -> divide (acc / n) rest
                | _ -> failwith "Invalid arguments for /"
            divide a rest
        | _ -> failwith "Invalid arguments for /")
]

// Add built-in functions to the global environment
for (name, func) in builtInFunctions do
    globalEnv.[name] <- func

let rec repl () =
    printf "Lisp> "
    let input = System.Console.ReadLine()
    if input <> "exit" then
        try
            let tokens = tokenize input
            let (expr, _) = readExpr tokens
            let result = eval expr globalEnv
            printfn "%A" result
        with
        | ex -> printfn "Error: %s" ex.Message
        repl ()

[<EntryPoint>]
let main argv = 
    printfn "Welcome to My Lisp Interpreter!"
    repl ()
    0
