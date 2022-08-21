#r "C:/Users/ChrisEvans/.nuget/packages/fparsec/1.1.1/lib/netstandard2.0/FParsec.dll"
#r "C:/Users/ChrisEvans/.nuget/packages/fparsec/1.1.1/lib/netstandard2.0/FParsecCS.dll"
open System
open FParsec
open FParsec.CharParsers

let ws = spaces // skips any whitespace

let str_ws s = pstring s >>. ws


type BinaryOperator =
    | Add
    | Sub
    | Mul
    | Div
    | Pow
    | Eql

type UnaryOperator =
    | Neg
    | Exp
    | Log

type Operator<'a> = {token:string;op:'a;wrap:bool}


type MathExpression =
    | Number of float
    | Bin of MathExpression * Operator<BinaryOperator> * MathExpression
    | Una of MathExpression * Operator<UnaryOperator>
    | Invalid of string

// we calculate with double precision floats
let number = pfloat .>> ws |>> Number

// we set up an operator precedence parser for parsing the arithmetic expressions
let opp = new OperatorPrecedenceParser<MathExpression,unit,unit>()
let expr = opp.ExpressionParser
opp.TermParser <- number <|> between (str_ws "(") (str_ws ")") expr

// operator definitions follow the schema
// operator type, string, trailing whitespace parser, precedence, associativity, function to apply

let add = {token="+";op=Add;wrap=true}
let sub = {token="-";op=Sub;wrap=true}
let mul = {token="*";op=Mul;wrap=true}
let div = {token="/";op=Div;wrap=true}
let pow = {token="^";op=Pow;wrap=true}
let neg =  {token="-";op=Neg;wrap=true}
let eql = {token="=";op=Eql;wrap=false}
let log =  {token="log";op=Log;wrap=true}
let exp =  {token="exp";op=Exp;wrap=true}

opp.AddOperator(InfixOperator("+", ws, 2, Associativity.Left, fun x y -> Bin(x,add,y)))
opp.AddOperator(InfixOperator("-", ws, 2, Associativity.Left, fun x y -> Bin(x, sub,y)))
opp.AddOperator(InfixOperator("*", ws, 3, Associativity.Left, fun x y -> Bin(x, mul,y)))
opp.AddOperator(InfixOperator("/", ws, 3, Associativity.Left, fun x y -> Bin(x, div,y)))
opp.AddOperator(InfixOperator("^", ws, 4, Associativity.Right, fun x y -> Bin(x,pow,y)))
opp.AddOperator(PrefixOperator("-", ws, 5, true, fun x -> Una(x,neg)))

opp.AddOperator(InfixOperator("=", ws, 1, Associativity.Left, fun x y -> Bin(x,eql,y)))

// we also want to accept the operators "exp" and "log", but we don't want to accept
// expressions like "logexp" 2, so we require that non-symbolic operators are not
// followed by letters

let ws1 = nextCharSatisfiesNot isLetter >>. ws
opp.AddOperator(PrefixOperator("log", ws1, 5, true, fun x -> Una(x,log)))
opp.AddOperator(PrefixOperator("exp", ws1, 5, true, fun x -> Una(x,exp)))

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

type ExpressionResult =
    | EqualResult of float * float * bool
    | MathResult of float

let rec doMath tree =

    let get x =
        match doMath x with
        | MathResult v -> v
        | _ -> nan

    match tree with
    | Number n -> MathResult n
    | Una (x,o) -> 
        match o.op with
        | Neg -> MathResult(-(get x))
        | Log -> MathResult(get x |> System.Math.Log)
        | Exp -> MathResult (Math.Exp(get x))
    | Bin (x, o,y) ->
        match o.op with
        | Mul -> MathResult(get x * get y)
        | Add -> MathResult(get x + get y)
        | Div -> MathResult(get x / get y)
        | Sub -> MathResult(get x - get y)
        | Pow -> MathResult(System.Math.Pow(get x, get y))
        | Eql -> 
            let v1 = get x
            let v2 = get y
            EqualResult (v1, v2, v1 = v2)
    | Invalid _ -> MathResult nan
    


let rec displayTree tree =
    match tree with
    | Number n -> sprintf "%g" n
    | Bin (x,o,y) -> 
        if o.wrap then
            sprintf "(%s%s%s)" (displayTree x) o.token (displayTree y)
        else
            sprintf "%s%s%s" (displayTree x) o.token (displayTree y)

    | Una (x,o) ->
        if o.wrap then
            sprintf "%s(%s)" o.token (displayTree x)
        else
            sprintf "%s%s" o.token (displayTree x)

    | Invalid msg -> msg

let example1 = "(-1+2)*10/2^2=4"
getTree example1 |> printfn "%O"
getTree example1 |> doMath |> printfn "%O"
getTree example1 |> displayTree |> printfn "%s"

let complexExp v o1 o2 exp =
    Bin(Bin(v, o1, exp), o2, v)

let complexExpU o1 o2 exp =
    Una(Una(exp, o2), o1)

let rec complexify tree =
    let random = Random()
    match tree with
    | Una (x,o) -> Una (complexify x, o)
    | Bin (x,o,y) -> 
        match o.op with
        | Eql -> Bin (complexify x, o, y)
        | _ ->
            let value() = Number (float(random.Next(-100,100)))

            let org = Bin(x,o,y)
            //Bin(Bin(value, mul, Bin(x,o,y)), div, value)
            complexExp (value()) mul div org 
            |> complexExp (value()) add sub
            |> complexExpU log exp
        
    | Invalid _ -> tree
    | Number _ -> tree

let example2 = "1+2"

getTree example2 |> complexify |> displayTree |> printfn "%s"
getTree example2 |> complexify |> doMath |> printfn "%O"
getTree example2 |> doMath |> printfn "%O"

let examples = [
    "1+2=3"
    "1+2*2=5"
    "(1+2)*2=6"
    "1+2/2=2"
    "2^3+1=9"
    "9-3+1=7"
    "(10*(1+2*2))/10=5"
    "5*(1+2*2)/5=5"
]

for e in examples do
    getTree e |> doMath |> printfn "%O"
    getTree e |> complexify |> displayTree |> printfn "%s"
    getTree e |> complexify |> doMath |> printfn "%O"

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