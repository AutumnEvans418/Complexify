module Runner
open Parser
open System

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
    | Id _ -> MathResult nan

let displayResult result =
    match result with
    | EqualResult (x,y,b) -> sprintf "%b" b
    | MathResult v -> sprintf "%g" v

let rec displayTree tree =
    match tree with
    | Number n -> sprintf "%g" n
    | Bin (x,o,y) -> 
        if o.wrap then
            sprintf "(%s%s%s)" (displayTree x) o.token (displayTree y)
        else
            sprintf "%s%s%s" (displayTree x) o.token (displayTree y)
    | Una (x,o) ->
        sprintf "(%s(%s))" o.token (displayTree x)
    | Invalid msg -> msg
    | Id str -> str


let private complexExp v o1 o2 exp =
    Bin(Bin(v, o1, exp), o2, v)


let private complexExp2 v v1 o1 o2 exp =
    Bin(Bin(v, o1, exp), o2, v1)

let private complexExpU o1 o2 exp =
    Una(Una(exp, o2), o1)

let rec private getVariables tree list =
    match tree with
    | Id a -> a::list
    | Number _ -> list
    | Invalid _ -> list
    | Bin (x,_,y) -> getVariables x list |> List.append (getVariables y list)
    | Una (x, _) -> getVariables x list

let rec complexify tree =
    let random = Random()
    let variables = getVariables tree []
    match tree with
    | Una (x,o) -> Una (complexify x, o)
    | Bin (x,o,y) -> 
        match o.op with
        | Eql -> Bin (complexify x, o, y)
        | _ ->
            
            let value() = 
                let num = Number (float(random.Next(1,100)))
                if variables.Length > 0 then
                    [num;Id (variables |> List.item (random.Next(0, variables.Length)))] |> List.item (random.Next(0, 2))
                else
                    num
            let org = Bin(x,o,y)
            
            let v = float(random.Next(0,100))
            let options = [
                complexExp (value()) mul div org     
                complexExp (value()) add sub org
                //complexExp2 (Number v) (Number (1./v)) pow pow org
            ]
            //Bin(Bin(value, mul, Bin(x,o,y)), div, value)
            options |> List.item (random.Next(0, options.Length))
            
            
        
    | Invalid _ -> tree
    | Number _ -> tree
    | Id _ -> tree

let rec complexifyRepeat (iter:int) (tree:MathExpression) =
    if iter > 0 then complexifyRepeat (iter-1) (complexify tree)
    else tree