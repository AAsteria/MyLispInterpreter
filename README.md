# MyLispInterpreter
A simple interpreter for Lisp written in F#

## Build & Run the Project
```bash
dotnet build
dotnet run
```

## Features
### Arithmetic Operations

-   Addition (`+`), subtraction (`-`), multiplication (`*`), division (`/`)
-   Supports arbitrary-precision integers (`bigint`)

### Variables and Functions

-   `define` to bind symbols to values or functions
-   `lambda` for creating anonymous functions

### Control Structures

-   Conditional expressions with `if`
-   Sequence execution with `begin`

### Macros

-   Define macros using `defmacro`
-   Macro expansion with parameter substitution

### Error Handling

-   Exception handling with `try`, `catch`, and `throw`

### List Operations

-   `list`, `cons`, `car`, `cdr`, `append`
-   Functional operations: `map`, `filter`, `reduce`

### Strings

-   String literals with double quotes
-   String functions: `string-length`, `string-append`, `substring`, `string->list`

### Boolean Values

-   `true` and `false` literals
-   Logical operations with `not`, `and`, `or`

### Tail Call Optimization

-   Supports proper tail recursion for efficient recursion

### Built-in Functions

-   Symbol creation with `symbol`
-   Output with `print`

## Testing Examples

### Arithmetic Operations

```bash
Lisp> (+ 1 2 3 4 5)
Number 15

Lisp> (* 2 3 4)
Number 24

Lisp> (- 10 3 2)
Number 5

Lisp> (/ 100 5 2)
Number 10 
```

### Variables and Functions

```bash
Lisp> (define x 10)
Symbol "x"

Lisp> x
Number 10

Lisp> (define square (lambda (n) (* n n)))
Symbol "square"

Lisp> (square 5)
Number 25

```
### Control Structures

```bash
Lisp> (if true "Yes" "No")
String "Yes"

Lisp> (define n 5)
Symbol "n"

Lisp> (if (= n 5) (print "n is five") (print "n is not five"))
n is five
Bool true
```

### Macros

```bash
Lisp> (defmacro when (condition body)
         (list (symbol "if") condition body))
Symbol "when"

Lisp> (when true (print "Condition is true"))
Condition is true
Bool true

Lisp> (defmacro unless (condition body)
         (list (symbol "if") (list (symbol "not") condition) body))
Symbol "unless"

Lisp> (unless false (print "Condition is false"))
Condition is false
Bool true
```

### Error Handling

```bash
Lisp> (try (/ 1 0) (catch e (print (string-append "Error: " e))))
Error: Division by zero
Bool true
```

### List Operations

```bash
Lisp> (define lst (list 1 2 3 4 5))
Symbol "lst"

Lisp> (car lst)
Number 1

Lisp> (cdr lst)
List [Number 2; Number 3; Number 4; Number 5]

Lisp> (cons 0 lst)
List [Number 0; Number 1; Number 2; Number 3; Number 4; Number 5]

Lisp> (append lst (list 6 7 8))
List [Number 1; Number 2; Number 3; Number 4; Number 5; Number 6; Number 7; Number 8]

Lisp> (map (lambda (x) (* x x)) lst)
List [Number 1; Number 4; Number 9; Number 16; Number 25]

```

### Strings

```bash
Lisp> (define greeting "Hello, World!")
Symbol "greeting"

Lisp> (string-length greeting)
Number 13

Lisp> (substring greeting 7 5)
String "World"

Lisp> (string-append greeting " How are you?")
String "Hello, World! How are you?"
```

### Tail-Recursive Factorial Function

```bash
Lisp> (define fact (lambda (n acc)
         (if (= n 0)
             acc
             (fact (- n 1) (* n acc)))))
Symbol "fact"

Lisp> (fact 5 1)
Number 120
```

## License
Notice that this project is under MIT License.