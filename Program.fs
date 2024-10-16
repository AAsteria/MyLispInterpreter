open System.Text.RegularExpressions
open System.Collections.Generic

type LispVal =
    | Symbol of string
    | Number of int
    | List of LispVal list
    | DottedList of LispVal list * LispVal
    | String of string
    | Bool of bool
    | Func of string list * LispVal list * Env
    | PrimitiveFunc of (LispVal list -> LispVal)

and Env = Dictionary<string, LispVal>

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
    | String s -> String s
    | Bool b -> Bool b
    | List [Symbol "quote"; xs] -> xs
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
        Func (paramNames, body, env)
    | List (funcExpr :: args) ->
        let funcVal = eval funcExpr env
        let argVals = args |> List.map (fun arg -> eval arg env)
        apply funcVal argVals
    | _ -> expr

and evalBody env body =
    match body with
    | [] -> failwith "No body in lambda"
    | exprs ->
        exprs |> List.map (fun expr -> eval expr env) |> List.last 

and apply func args =
    match func with
    | PrimitiveFunc f -> f args
    | Func (prms, body, closureEnv) ->
        if List.length prms <> List.length args then
            failwithf "Incorrect number of arguments: expected %d, got %d" (List.length prms) (List.length args)
        else
            let localEnv = Dictionary<string, LispVal>(closureEnv)
            List.iter2 (fun param arg -> localEnv.[param] <- arg) prms args
            evalBody localEnv body
    | _ -> failwithf "Unknown function: %A" func

and defineFunction name value (env: Env) =
    env.[name] <- value
    printfn "Defined function %s" name

let builtInFunctions = [
    "+", PrimitiveFunc (fun args ->
        let rec sum acc lst =
            match lst with
            | [] -> Number acc
            | Number n :: rest -> sum (acc + n) rest
            | _ -> failwith "Invalid arguments for +"
        sum 0 args)
    "-", PrimitiveFunc (fun args ->
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
    "*", PrimitiveFunc (fun args ->
        let rec prod acc lst =
            match lst with
            | [] -> Number acc
            | Number n :: rest -> prod (acc * n) rest
            | _ -> failwith "Invalid arguments for *"
        prod 1 args)
    "/", PrimitiveFunc (fun args ->
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
    "=", PrimitiveFunc (fun args ->
        match args with
        | [Number a; Number b] -> Bool (a = b)
        | _ -> failwith "Invalid arguments for =")
    "cons", PrimitiveFunc (fun args ->
        match args with
        | [x; List lst] -> List (x :: lst)
        | [x; y] -> DottedList ([x], y)
        | _ -> failwith "Invalid arguments for cons")
    "car", PrimitiveFunc (fun args ->
        match args with
        | [List (x :: _)] -> x
        | [DottedList (x :: _, _)] -> x
        | _ -> failwith "Invalid arguments for car")
    "cdr", PrimitiveFunc (fun args ->
        match args with
        | [List (_ :: xs)] -> List xs
        | [DottedList ([_], tail)] -> tail
        | [DottedList (_ :: xs, tail)] -> DottedList (xs, tail)
        | _ -> failwith "Invalid arguments for cdr")
    "list", PrimitiveFunc (fun args -> List args)
    "length", PrimitiveFunc (fun args ->
        match args with
        | [List lst] -> Number (List.length lst)
        | _ -> failwith "Invalid arguments for length")
    "append", PrimitiveFunc (fun args ->
        match args with
        | [List lst1; List lst2] -> List (lst1 @ lst2)
        | _ -> failwith "Invalid arguments for append")
    "map", PrimitiveFunc (fun args ->
        match args with
        | [func; List lst] ->
            let results = lst |> List.map (fun x -> apply func [x])
            List results
        | _ -> failwith "Invalid arguments for map")
    "filter", PrimitiveFunc (fun args ->
        match args with
        | [func; List lst] ->
            let results = lst |> List.filter (fun x ->
                match apply func [x] with
                | Bool true -> true
                | Bool false -> false
                | _ -> failwith "Filter function must return a boolean")
            List results
        | _ -> failwith "Invalid arguments for filter")
    "reduce", PrimitiveFunc (fun args ->
        match args with
        | [func; initial; List lst] ->
            let result = lst |> List.fold (fun acc x -> apply func [acc; x]) initial
            result
        | _ -> failwith "Invalid arguments for reduce")
]

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
