#r "C:/Users/ChrisEvans/.nuget/packages/fparsec/1.1.1/lib/netstandard2.0/FParsec.dll"
#r "C:/Users/ChrisEvans/.nuget/packages/fparsec/1.1.1/lib/netstandard2.0/FParsecCS.dll"
open System
open FParsec
open FParsec.CharParsers

let ws = spaces // skips any whitespace

let str_ws s = pstring s >>. ws


type MathExpression =
    | Number of float
    | Add of MathExpression * MathExpression
    | Sub of MathExpression * MathExpression
    | Mul of MathExpression * MathExpression
    | Div of MathExpression * MathExpression
    | Pow of MathExpression * MathExpression
    | Neg of MathExpression
    | Exp of MathExpression
    | Log of MathExpression
    | Invalid of string
    | Equal of MathExpression * MathExpression
// we calculate with double precision floats
let number = pfloat .>> ws |>> Number

// we set up an operator precedence parser for parsing the arithmetic expressions
let opp = new OperatorPrecedenceParser<MathExpression,unit,unit>()
let expr = opp.ExpressionParser
opp.TermParser <- number <|> between (str_ws "(") (str_ws ")") expr

// operator definitions follow the schema
// operator type, string, trailing whitespace parser, precedence, associativity, function to apply

opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, fun x y -> Add (x,y)))
opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, fun x y -> Sub(x,y)))
opp.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, fun x y -> Mul (x,y)))
opp.AddOperator(InfixOperator("/", ws, 2, Associativity.Left, fun x y -> Div (x,y)))
opp.AddOperator(InfixOperator("^", ws, 3, Associativity.Right, fun x y -> Pow(x, y)))
opp.AddOperator(PrefixOperator("-", ws, 4, true, fun x -> Neg(x)))

opp.AddOperator(InfixOperator("=", ws, 5, Associativity.None, fun x y -> Equal(x,y)))

// we also want to accept the operators "exp" and "log", but we don't want to accept
// expressions like "logexp" 2, so we require that non-symbolic operators are not
// followed by letters

let ws1 = nextCharSatisfiesNot isLetter >>. ws
opp.AddOperator(PrefixOperator("log", ws1, 4, true, fun x -> Log x))
opp.AddOperator(PrefixOperator("exp", ws1, 4, true, fun x -> Exp x))

let completeExpression = ws >>. expr .>> eof // we append the eof parser to make
                                            // sure all input is consumed

// running and testing the parser
/////////////////////////////////
let calculate s = run completeExpression s

let getTree  s=
    match calculate s with
    | Success (v,_,_) -> v
    | Failure (msg,_,_) -> Invalid msg

getTree "(1+2)*3" |> printfn "%O"

let rec doMath tree =
    match tree with
    | Number n -> n
    | Mul (x, y) -> doMath x * doMath y
    | Add (x,y) -> doMath x + doMath y
    | Invalid msg -> nan
    | Div (x,y) -> doMath x / doMath y
    | Sub (x,y) -> doMath x - doMath y
    | Exp x -> doMath x |> System.Math.Exp
    | Log x -> doMath x |> System.Math.Log
    | Neg x -> -(doMath x)
    | Pow (x,y) -> System.Math.Pow(doMath x, doMath y)


let rec displayTree tree =
    match tree with
    | Number n -> sprintf "%g" n
    | Mul (x,y) -> sprintf "(%s*%s)" (displayTree x) (displayTree y)
    | Add (x,y) -> sprintf "(%s+%s)" (displayTree x) (displayTree y)
    | Invalid msg -> msg
    | Div (x,y) -> sprintf "(%s/%s)" (displayTree x) (displayTree y)
    | Sub (x,y) -> sprintf "(%s-%s)" (displayTree x) (displayTree y)
    | Exp x -> sprintf "exp(%s)" (displayTree x)
    | Log x -> sprintf "log(%s)" (displayTree x)
    | Neg x -> sprintf "-%s" (displayTree x)
    | Pow (x,y) -> sprintf "(%s)^(%s)" (displayTree x) (displayTree y)

let example1 = "(-1+2)*10/2^2=4"
getTree example1 |> printfn "%O"
getTree example1 |> doMath |> printfn "%f"
getTree example1 |> displayTree |> printfn "%s"


// let equals expectedValue r =
//     match r with
//     | Success (v, _, _) when v = expectedValue -> ()
//     | Success (v, _, _)     -> failwith "Math is hard, let's go shopping!"
//     | Failure (msg, err, _) -> printf "%s" msg; failwith msg

// let test() =
//     calculate "10.5 + 123.25 + 877"  |> equals 1010.75
//     calculate "10/2 + 123.125 + 877" |> equals 1005.125
//     calculate "(123 + log 1 + 877) * 9/3" |> equals 3000.
//     calculate " ( ( exp 0 + (6 / ( 1 +2 ) )- 123456 )/ 2+123 + 877) * 3^2 / 3" |> equals (-182179.5)
//     printfn "No errors"

// // currently the program only executes some tests
// do test()