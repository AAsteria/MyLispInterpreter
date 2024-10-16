open System.Text.RegularExpressions
open System.Collections.Generic
open System.Numerics

type LispVal =
    | Symbol of string
    | Number of bigint
    | String of string
    | Bool of bool
    | List of LispVal list
    | DottedList of LispVal list * LispVal
    | Func of string list * LispVal list * Env
    | PrimitiveFunc of (LispVal list -> LispVal)
    | Macro of string list * LispVal list

and Env = Dictionary<string, LispVal>

type TailCall =
    | Value of LispVal
    | Thunk of (unit -> TailCall)

let globalEnv: Env = Dictionary<string, LispVal>()

let rec tokenize (input: string): string list =
    let pattern = @"[\s,]*('|\[|\]|[()]|""(?:\\.|[^""])*""|[^\s\[\]()'""]+)"
    [ for m in Regex.Matches(input, pattern) -> m.Groups.[1].Value ]

let rec readExpr tokens =
    let rec readExpr' tokens acc =
        match tokens with
        | [] -> failwith "Unexpected end of input"
        | "'" :: rest ->
            let (expr, remainingTokens) = readExpr rest
            (List [Symbol "quote"; expr], remainingTokens)
        | "(" :: rest ->
            let (listExpr, remainingTokens) = parseList rest
            (listExpr, remainingTokens)
        | ")" :: _ -> failwith "Unexpected )"
        | token :: rest ->
            let atom =
                match token with
                | s when s.StartsWith("\"") && s.EndsWith("\"") ->
                    String (s.Substring(1, s.Length - 2).Replace("\\\"", "\""))
                | "true" -> Bool true
                | "false" -> Bool false
                | _ when Regex.IsMatch(token, @"^-?\d+$") -> Number (BigInteger.Parse token)
                | _ -> Symbol token
            (atom, rest)
    and parseList tokens =
        let rec parseList' acc tokens =
            match tokens with
            | [] -> failwith "Unexpected end of input"
            | ")" :: rest -> (List (List.rev acc), rest)
            | tokenList ->
                let (expr, remainingTokens) = readExpr' tokenList []
                parseList' (expr :: acc) remainingTokens
        parseList' [] tokens
    readExpr' tokens []
    
let rec eval expr (env: Env) : TailCall =
    match expr with
    | Symbol s ->
        match env.TryGetValue(s) with
        | true, value -> Value value
        | false, _ -> failwithf "Unknown symbol: %s" s
    | Number n -> Value (Number n)
    | String s -> Value (String s)
    | Bool b -> Value (Bool b)
    | List [Symbol "quote"; xs] -> Value xs
    | List _ ->
        let expandedExpr = expandMacros expr env
        evalExpanded expandedExpr env
    | _ -> Value expr

and evalExpanded expr env =
    match expr with
    | List [Symbol "if"; test; conseq] ->
        let alt = Bool false
        match eval test env with
        | Value (Bool true) -> Thunk(fun () -> eval conseq env)
        | Value (Bool false) -> Thunk(fun () -> eval alt env)
        | _ -> failwith "Invalid condition in if"
    | List [Symbol "if"; test; conseq; alt] ->
        match eval test env with
        | Value (Bool true) -> Thunk(fun () -> eval conseq env)
        | Value (Bool false) -> Thunk(fun () -> eval alt env)
        | _ -> failwith "Invalid condition in if"
    | List (Symbol "if" :: test :: conseq :: alt :: rest) ->
        failwith "if requires exactly 2 or 3 arguments"
    | List (Symbol "define" :: Symbol name :: expr :: []) ->
        match eval expr env with
        | Value value ->
            env.[name] <- value
            Value (Symbol name)
        | Thunk f ->
            match f () with
            | Value value ->
                env.[name] <- value
                Value (Symbol name)
            | _ -> failwith "Invalid value in define"
    | List (Symbol "lambda" :: List prms :: body) ->
        let paramNames = List.map (function Symbol name -> name | _ -> failwith "Invalid parameter") prms
        Value (Func (paramNames, body, env))
    | List (Symbol "defmacro" :: Symbol name :: List prms :: body) ->
        let paramNames = List.map (function Symbol n -> n | _ -> failwith "Invalid parameter") prms
        let macro = Macro (paramNames, body)
        env.[name] <- macro
        Value (Symbol name)
    | List (Symbol "try" :: tryExpr :: List [Symbol "catch"; Symbol exVar; catchExpr] :: []) ->
        try
            eval tryExpr env
        with ex ->
            let localEnv = Dictionary<string, LispVal>(env)
            localEnv.[exVar] <- String ex.Message
            Thunk(fun () -> eval catchExpr localEnv)
    | List (Symbol "throw" :: expr :: []) ->
        match eval expr env with
        | Value (String msg) -> failwith msg
        | _ -> failwith "throw requires a string message"
    | List (funcExpr :: args) ->
        match eval funcExpr env with
        | Value funcVal ->
            let argVals = args |> List.map (fun arg ->
                match eval arg env with
                | Value v -> v
                | Thunk f -> runEval (f ()))
            apply funcVal argVals
        | Thunk f ->
            match runEval (f ()) with
            | funcVal ->
                let argVals = args |> List.map (fun arg ->
                    match eval arg env with
                    | Value v -> v
                    | Thunk f -> runEval (f ()))
                apply funcVal argVals
    | _ -> Value expr

and expandMacros expr env =
    let rec expand expr =
        match expr with
        | List (Symbol name :: args) when env.ContainsKey(name) ->
            match env.[name] with
            | Macro (prms, body) ->
                if List.length prms <> List.length args then
                    failwithf "Macro %s expects %d arguments, got %d" name (List.length prms) (List.length args)
                else
                    let localEnv = Dictionary<string, LispVal>(env)
                    List.iter2 (fun param arg -> localEnv.[param] <- arg) prms args
                    match evalBody localEnv body with
                    | Value v -> expand v
                    | Thunk f -> expand (runEval (f ()))
            | _ -> List (List.map expand (Symbol name :: args))
        | List lst -> List (List.map expand lst)
        | _ -> expr
    expand expr

and evalBody env body : TailCall =
    match body with
    | [] -> failwith "No body in lambda"
    | exprs ->
        let rec evalSequence exprs =
            match exprs with
            | [] -> failwith "Empty body"
            | [last] -> eval last env
            | expr :: rest ->
                match eval expr env with
                | Value _ -> evalSequence rest
                | Thunk f -> f () |> ignore; evalSequence rest
        evalSequence exprs

and apply func args : TailCall =
    match func with
    | PrimitiveFunc f -> Value (f args)
    | Func (prms, body, closureEnv) ->
        if List.length prms <> List.length args then
            failwithf "Incorrect number of arguments: expected %d, got %d" (List.length prms) (List.length args)
        else
            let localEnv = Dictionary<string, LispVal>(closureEnv)
            List.iter2 (fun param arg -> localEnv.[param] <- arg) prms args
            Thunk(fun () -> evalBody localEnv body)
    | _ -> failwithf "Unknown function: %A" func

and runEval (tc: TailCall) : LispVal =
    match tc with
    | Value v -> v
    | Thunk f -> runEval (f ())

let builtInFunctions = [
    "+", PrimitiveFunc (fun args ->
        let rec sum acc lst =
            match lst with
            | [] -> Number acc
            | Number n :: rest -> sum (acc + n) rest
            | _ -> failwith "Invalid arguments for +"
        sum 0I args)
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
        prod 1I args)
    "/", PrimitiveFunc (fun args ->
        match args with
        | [] -> failwith "Invalid arguments for /"
        | [Number a] ->
            if a = 0I then failwith "Division by zero"
            else Number (1I / a)
        | Number a :: rest ->
            let rec divide acc lst =
                match lst with
                | [] -> Number acc
                | Number n :: rest ->
                    if n = 0I then failwith "Division by zero"
                    else divide (acc / n) rest
                | _ -> failwith "Invalid arguments for /"
            divide a rest
        | _ -> failwith "Invalid arguments for /")
    "=", PrimitiveFunc (fun args ->
        match args with
        | [Number a; Number b] -> Bool (a = b)
        | [String a; String b] -> Bool (a = b)
        | [Bool a; Bool b] -> Bool (a = b)
        | _ -> failwith "Invalid arguments for =")
    "print", PrimitiveFunc (fun args ->
        args |> List.iter (fun arg ->
            match arg with
            | String s -> printf "%s" s
            | Number n -> printf "%A" n
            | Bool b -> printf "%b" b
            | _ -> printf "%A" arg)
        printfn ""
        Bool true)
    "symbol", PrimitiveFunc (fun args ->
        match args with
        | [String s] -> Symbol s
        | _ -> failwith "Invalid arguments for symbol")
    "string-length", PrimitiveFunc (fun args ->
        match args with
        | [String s] -> Number (bigint s.Length)
        | _ -> failwith "Invalid arguments for string-length")
    "string-append", PrimitiveFunc (fun args ->
        let rec append acc lst =
            match lst with
            | [] -> String acc
            | String s :: rest -> append (acc + s) rest
            | _ -> failwith "Invalid arguments for string-append"
        append "" args)
    "substring", PrimitiveFunc (fun args ->
        match args with
        | [String s; Number start; Number length] ->
            let start = int start
            let length = int length
            if start < 0 || length < 0 || start + length > s.Length then
                failwith "Invalid start or length in substring"
            else
                String (s.Substring(start, length))
        | _ -> failwith "Invalid arguments for substring")
    "string->list", PrimitiveFunc (fun args ->
        match args with
        | [String s] ->
            let chars = s.ToCharArray() |> Array.toList |> List.map (fun c -> String (c.ToString()))
            List chars
        | _ -> failwith "Invalid arguments for string->list")
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
        | [List lst] -> Number (bigint (List.length lst))
        | _ -> failwith "Invalid arguments for length")
    "append", PrimitiveFunc (fun args ->
        match args with
        | [List lst1; List lst2] -> List (lst1 @ lst2)
        | _ -> failwith "Invalid arguments for append")
    "map", PrimitiveFunc (fun args ->
        match args with
        | [func; List lst] ->
            let results = lst |> List.map (fun x ->
                match apply func [x] with
                | Value v -> v
                | Thunk f -> runEval (f ()))
            List results
        | _ -> failwith "Invalid arguments for map")
    "filter", PrimitiveFunc (fun args ->
        match args with
        | [func; List lst] ->
            let results = lst |> List.filter (fun x ->
                match apply func [x] with
                | Value (Bool true) -> true
                | Value (Bool false) -> false
                | Thunk f ->
                    match runEval (f ()) with
                    | Bool true -> true
                    | Bool false -> false
                    | _ -> failwith "Filter function must return a boolean"
                | _ -> failwith "Filter function must return a boolean")
            List results
        | _ -> failwith "Invalid arguments for filter")
    "reduce", PrimitiveFunc (fun args ->
        match args with
        | [func; initial; List lst] ->
            let folder acc x =
                match apply func [acc; x] with
                | Value v -> v
                | Thunk f -> runEval (f ())
            let result = List.fold folder initial lst
            result
        | _ -> failwith "Invalid arguments for reduce")
]

for (name, func) in builtInFunctions do
    globalEnv.[name] <- func

let rec repl () =
    printf "Lisp> "
    let input = System.Console.ReadLine()
    if input <> null && input <> "exit" then
        try
            let tokens = tokenize input
            let (expr, _) = readExpr tokens
            let result = eval expr globalEnv
            let finalResult = runEval result
            printfn "%A" finalResult
        with
        | ex -> printfn "Error: %s" ex.Message
        repl ()
    else
        ()

[<EntryPoint>]
let main argv =
    printfn "Welcome to My Lisp Interpreter!"
    repl ()
    0