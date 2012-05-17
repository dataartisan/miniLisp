(*
Unit tests to see if everything is good.
First, lets check if we broke anything from 2a.
Then, new Test Cases
*)

module Tests
open Interpreter

open NUnit.Framework
open Interpreter
    
let complex1 = eval(If(Const 7,Const 3,Const 4))
let complexResult1 = Const 3
    
let complex2 = eval(Div(Minus(Plus(Const 1, Const 2),Times(Const 3,Const 4)),Const 5))
let complexResult2 = Const -1

let complex3 = eval (If(Fun("x",Var "x"),Const 3,Const 4))
let complexResult3 = Const 3

let complex4 = eval(If(Const 0,Const 3,Const 4))
let complexResult4 = Const 4

let complex5 = eval(Fun("x",Plus(Const 7,Var("x"))))
let complexResult5 = Fun("x",Plus(Const 7,Var("x")))
    
let complex6 = eval(App(Fun("x",Plus(Const 7,Var("x"))),Const 3))
let complexResult6 = Const 10

let complex7 = eval (If(Fun("x",Var "y"),Const 3,Const 4))
let complexResult7 = Const 3

let complex8 = eval (App(App(Y, Fun("f",Fun("n",If(Var "n",Times(Var "n",App(Var("f"),Minus(Var "n",Const 1))),Const 1)))), Const 5))
let complexResult8 = Const 120

//new from Test
let complex9 = eval (Cell(Plus(Const 3, Const 7), Const 5))
let complexResult9 = Cell(Const 10, Const 5)

let complex10 = eval (Cell(Const 1, Cell(Const 2, Cell(Const 3, Const 0))))
let complexResult10 = Cell(Const 1, Cell(Const 2, Cell(Const 3, Const 0)))

let complex11 = eval (Car(Cell(Const 10, Const 5)))
let complexResult11 = Const 10

let complex12 = eval (Cdr(Cell(Const 10, Const 5)))
let complexResult12 = Const 5

let complex13 = eval (Cdr(Cell(Const 10, Fun("x", Var "x"))))
let complexResult13 =  Fun("x", Var "x")

let complex14 = IsEmptyExpressionReturner(Cell(Const 0, Const 10))
let complexResult14 = Const 1

let complex15 = IsEmptyExpressionReturner(Cell(Const 0, Const 0))
let complexResult15 = Const 0


[<TestFixture>] 
    type ProjectTests() = 
        
        [<Test>] member test.
         ``ComplexGroup1`` () =
            Assert.AreEqual(complexResult1, complex1)
        
        [<Test>] member test.
         ``ComplexGroup2`` () =
            Assert.AreEqual(complexResult2, complex2)

        [<Test>] member test.
         ``ComplexGroup3`` () =
            Assert.AreEqual(complexResult3, complex3)
        
        [<Test>] member test.
         ``ComplexGroup4`` () =
            Assert.AreEqual(complexResult4, complex4)

        [<Test>] member test.
         ``ComplexGroup5`` () =
            Assert.AreEqual(complexResult5, complex5)

        [<Test>] member test.
         ``ComplexGroup6`` () =
            Assert.AreEqual(complexResult6, complex6)

        [<Test>] member test.
         ``ComplexGroup7`` () =
            Assert.AreEqual(complexResult7, complex7)
                
        [<Test>] member test.
         ``ComplexGroup8`` () =
            Assert.AreEqual(complexResult8, complex8)

        [<Test>] member test.
         ``ComplexGroup9`` () =
            Assert.AreEqual(complexResult9, complex9)

        [<Test>] member test.
         ``ComplexGroup10`` () =
            Assert.AreEqual(complexResult10, complex10)

        [<Test>] member test.
         ``ComplexGroup11`` () =
            Assert.AreEqual(complexResult11, complex11)

         [<Test>] member test.
         ``ComplexGroup12`` () =
            Assert.AreEqual(complexResult12, complex12)

        [<Test>] member test.
         ``ComplexGroup13`` () =
            Assert.AreEqual(complexResult13, complex13)

         [<Test>] member test.
         ``ComplexGroup14`` () =
            Assert.AreEqual(complexResult14, complex14)

        [<Test>] member test.
         ``ComplexGroup15`` () =
            Assert.AreEqual(complexResult15, complex15)
