module Parser

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
opp.TermParser <- 
    number 
    <|> between (str_ws "(") (str_ws ")") expr
    <|> between (str_ws "[") (str_ws "]") expr

//todo: add brackets, square root
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
