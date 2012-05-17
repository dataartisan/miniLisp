module Interpreter
open System
//Abstract Syntax Tree is defined as follows
type Expr =
    | Const of int
    | Bool of bool
    | Var of string
    | Fun of string * Expr
    | Plus of Expr * Expr
    | Times of Expr * Expr
    | Minus of Expr * Expr
    | Div of Expr * Expr
    | If of Expr * Expr * Expr
    | App of Expr * Expr
    | Cell of Expr * Expr
    | Car of Expr
    | Cdr of Expr

    
//our subtitute function
//Idea was acquired from the ML Interpreter
let rec subst (x:String, v:Expr, a:Expr):Expr = match a with
    | Const _ -> a
    | Var y -> if x = y then v else a
    | Plus (x', y') -> Plus(subst(x, v, x'), subst(x ,v , y'))
    | Minus(x', y') -> Minus(subst(x, v, x'), subst(x, v, y'))
    | Times(x', y') -> Times(subst(x, v, x'), subst(x, v, y'))
    | Div(x', y') -> Div(subst(x, v, x'), subst(x, v, y'))
    | If(x', y', z') -> If(subst(x, v, x'), subst(x, v, y'), subst(x, v, z'))      
    | Fun(y', a') -> if x = y' then a else Fun(y', subst (x, v, a'))
    | App (x', y') -> App (subst(x, v, x'), subst(x, v, y'))
    | _ -> failwith "failure at substitution function. No match found."
    //| Y(x') -> Y(subst(x, v, x')) //Not sure this would be needed.
    
let rec eval(e: Expr) = match e with
        | Const c -> Const c
        | Bool(b) -> Bool(b)
        | Plus(e1, e2) -> match  (eval e1, eval e2) with
           | (Const x, Const y) -> Const(x + y)
           | _ -> Plus(eval (e1), eval(e2))
        | Minus(e1, e2) -> match (eval e1, eval e2) with
            | (Const x, Const y) -> Const(x - y)
            | _ -> Minus(eval (e1), eval(e2))
        | Times(e1, e2) -> match (eval e1, eval e2) with
            | (Const x, Const y) -> Const(x * y)
            | _ -> Times(eval (e1), eval(e2))
        | Div(e1, e2) -> match(eval e1, eval e2) with
            | (Const x, Const y) -> Const(x / y)
            | _ -> Times(eval (e1), eval(e2))
        | Var x -> failwith "error (* your eval function should no longer include a branch for Var  *)"
        | If (c, e1, e2) -> if eval c = Const 0 then eval e2 else eval e1
        | Fun(x, e1) -> Fun(x, e1)
        | App(e1, e2) -> match eval e1 with 
            | Fun(x, e) -> eval(subst (x, e2, e))
            //| Var _ -> e1           
        | Cell(e1, e2) -> 
            let e1' = eval e1 in
            let e2' = eval e2 in
            Cell(e1', e2')
        | Car(e1) -> match e1 with
            | Cell(e1', e2) -> e1'
            | _ -> failwith "Could not match to Car Expression"
        | Cdr(e1) -> match e1 with
            | Cell(e1', e2) -> e2    
            | _ -> failwith "Could not match to Cdr Expression"       
        //| Car(Cell(e1, e2)) -> e1
        //| Cdr(Cell(e1, e2)) -> e2        
        | _ -> failwith "I could not match the patterns"

let PreLength = Fun("f", Fun("c", If(Var "c", Plus(Const 1, App(Var "f", Cdr (Var "c"))), Const 0)))


//Y = lambda G. (lambda g. G(lambda x. g g x)) (lambda g. G(lambda x. g g x)) // Crazy expression to create here //
let Y = Fun("G", App(Fun("g", App(Var("G"), App(Var("g"), Var("g")))), Fun("g", App(Var("G"), App(Var("g"), Var("g"))))))


let IsEmptyExpressionReturner(e1) = match eval e1 with
           | Cell(e1', e2) -> if eval e1' = Const 0 then (if eval e2 = Const 0 then Const 0 else Const 1) else Const 1


let e1 = Cell(Plus(Const 3, Const 7), Const 5)
printfn "%A" (eval e1)

let e2 = Cell(Const 1, Cell(Const 2, Cell(Const 3, Const 0)))
printfn "%A" (eval e2)

let e3 = Car(Cell(Const 10, Const 5))
printfn "%A" (eval e3)

let e4 = Cdr(Cell(Const 10, Const 5))
printfn "%A" (eval e4) 

//testing one of the older expressions
let e5 = Cell(Plus(Const 3, Const 7), Const 5)
printfn "%A" (eval e5) 

//let e6 = Cdr(Const 10)
//printfn "%A" (eval e6) 
//above statement is incorrect

let e7 = Cdr(Cell(Const 10, Fun("x", Var "x")))
printfn "%A" (eval e7) 

let e8 = Cell(Const 1, Const 0)
printfn "%A" (eval e8) 

let e9 = Cell(Const 1, Cell(Const 2, Cell(Const 4, Cell(Const 8, Const 0))))
printfn "%A" (eval e9)

 
let e12 = IsEmptyExpressionReturner(Cell(Const 0, Const 10))
printfn "%A" (e12)

let e13 = IsEmptyExpressionReturner(Cell(Const 0, Const 0))
printfn "%A" (e13)

let e14 = Cell(Cell(Const 1, Cell(Const 2, Const 3)), Cell(Const 4, Const 0))   //App(Y preLength, Const 0)
printfn "%A" (eval e14)

let e15 = App(App(Y, Fun("f",Fun("n",If(Var "n",Times(Var "n",App(Var("f"),Minus(Var "n",Const 1))),Const 1)))), Const 5)
printfn "%A" (eval e15)

Console.ReadKey() |> ignore


