(* Programming language concepts for software developers, 2010-08-28 *)

(* Representing object language expressions using recursive datatypes *)
module Intro1 = 
    type expr = 
    | CstI of int
    | Prim of string * expr * expr;;

    let e0 = Prim("+", CstI 2, Prim("*", CstI 3, CstI 4))

    let e1 = CstI 17;;

    let e2 = Prim("-", CstI 3, CstI 4);;

    let e3 = Prim("+", Prim("*", CstI 7, CstI 9), CstI 10);;


    (* Evaluating expressions using recursive functions *)

    let rec eval (e : expr) : int =
        match e with
        | CstI i -> i
        | Prim("+", e1, e2) -> eval e1 + eval e2
        | Prim("*", e1, e2) -> eval e1 * eval e2
        | Prim("-", e1, e2) -> eval e1 - eval e2
        | Prim _            -> failwith "unknown primitive";;

    let e0v = eval e0;;
    let e1v = eval e1;;
    let e2v = eval e2;;
    let e3v = eval e3;;


    (* Changing the meaning of subtraction *)

    let rec evalm (e : expr) : int =
        match e with
        | CstI i -> i
        | Prim("+", e1, e2) -> evalm e1 + evalm e2
        | Prim("*", e1, e2) -> evalm e1 * evalm e2
        | Prim("-", e1, e2) -> 
        let res = evalm e1 - evalm e2
        if res < 0 then 0 else res 
        | Prim _            -> failwith "unknown primitive";;


    let e4v = evalm (Prim("-", CstI 10, CstI 27));;

    (* The Pretty Printer function *)

    let rec fmt (e : expr) : string =
    match e with
        CstI i -> i.ToString()
    | Prim("+", e1, e2) -> "(" + fmt e1 + "+" + fmt e2 + ")"
    | Prim("*", e1, e2) -> "(" + fmt e1 + "*" + fmt e2 + ")"
    | Prim("-", e1, e2) -> "(" + fmt e1 + "-" + fmt e2 + ")"
    | Prim _            -> failwith "fmt: unknown primitive";;
    
    (* Programming language concepts for software developers, 2010-08-28 *)

    (* Evaluating simple expressions with variables *)

module Intro2 =

    (* Association lists map object language variables to their values *)

    let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

    let emptyenv = []; (* the empty environment *)

    let rec lookup env x =
        match env with 
        | []        -> failwith (x + " not found")
        | (y, v)::r -> if x=y then v else lookup r x;;

    let cvalue = lookup env "c";;


    (* Object language expressions with variables *)

    type expr = 
    | CstI of int
    | Var of string
    | If of expr * expr * expr
    | Prim of string * expr * expr;;


    let e1 = CstI 17;;

    let e2 = Prim("+", CstI 3, Var "a");;

    let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;


    (* Evaluation within an environment *)

    //Exercise 1.1

    let rec eval e (env : (string * int) list) : int =
        match e with
        | CstI i            -> i
        | Var x             -> lookup env x 
        | If(e1, e2, e3) -> 
            let i1 = eval e1 env
            if i1 <> 0 then eval e2 env else eval e3 env
        | Prim(ope, e1, e2) ->
            let i1 = eval e1 env
            let i2 = eval e2 env
            match ope with 
            | "+" -> i1 + i2
            | "*" -> i1 * i2
            | "-" -> i1 * i2
            |"Max" -> if i1 > i2 then i1 else i2
            |"Min" -> if i1 < i2 then i1 else i2
            |"==" -> if i1 = i2 then 1 else 0
        | Prim _            -> failwith "unknown primitive";;

    let e1v  = eval e1 env;;
    let e2v1 = eval e2 env;;
    let e2v2 = eval e2 [("a", 314)];;
    let e3v  = eval e3 env;;

    let testInt = CstI 20;;

    let testInt2 = CstI 40;;

    let testMax  = Prim("Max", testInt, testInt2);;

    let testMin = Prim("Min", testInt, testInt2);;

    let testEqual = Prim("==", testInt, testInt2);;

    let testEqual2 = Prim("==", testInt, Var "e");;


    let evalTestMax = eval testMax env;;

    let evalTestMin = eval testMin env;;

    let evalTestEqual = eval testEqual env;;

    let evalTestEqual2 = eval testEqual2 [("e", 20)];;

    let testIf = If(Var "y", CstI 100, CstI 5000);;

    let evalTestIf = eval testIf [("y", 0)];;


    //Exercise 1.2


    // 1.2.1
    type aexpr = 
    | CstI of int
    | Var of string
    | Add of aexpr * aexpr
    | Mul of aexpr * aexpr
    | Sub of aexpr * aexpr

    // 1.2.2
    // Sub(Var "v", Add(Var "w", Var "z"))
    // Mul(CstI 2, Sub(Var "v", Add(Var "w", Var "z")))
    // Add(Add(Var "x", Var "y"), Add(Var "z", Var "v"))

    // 1.2.3
    let rec fmt aexpr: string =
        match aexpr with
        | CstI i -> string i
        | Var x -> x
        | Add (e1, e2) -> "(" + fmt e1 + " + " + fmt e2 + ")"
        | Sub (e1, e2) -> "(" + fmt e1 + " - " + fmt e2 + ")"
        | Mul (e1, e2) -> "(" + fmt e1 + " * " + fmt e2 + ")"


    //    
    let example1 = Sub(Var "x", CstI 34)
    let result1 = fmt example1
    // result1 should be "(x - 34)"

    let example2 = Add(Var "x", Mul(CstI 2, Var "y"))
    let result2 = fmt example2
    // result2 should be "(x + (2 * y))"

    let example3 = Mul(Add(Var "a", CstI 10), Sub(Var "b", CstI 5))
    let result3 = fmt example3
    // result3 should be "((a + 10) * (b - 5))"

    let exampleVar1 = Sub(Var "v", Add(Var "w", Var "z"))
    let resultVar1 = fmt exampleVar1
    // result should be "(v - (w + z))"

    let exampleVar2 = Mul(CstI 2, Sub(Var "v", Add(Var "w", Var "z")))
    let resultVar2 = fmt exampleVar2
    // result should be "(2 * (v - (w + z)))"

    let exampleVar3 = Add(Add(Var "x", Var "y"), Add(Var "z", Var "v"))
    let resultVar3 = fmt exampleVar3
    // result should be "((x + y) + (z + v))"

    // 1.2.4
    let rec simplify aexpr : aexpr = 
        match aexpr with
        | Add(CstI 0, e2) -> simplify e2
        | Add(e1, CstI 0) -> simplify e1
        | Sub(e1, CstI 0) -> simplify e1
        | Mul(CstI 1, e2) -> simplify e2
        | Mul(e1, CstI 1) -> simplify e1
        | Mul(CstI 0, _) -> CstI 0 
        | Mul(_, CstI 0) -> CstI 0
        | Add(e1, e2) ->
            let i1 = simplify e1
            let i2 = simplify e2
            if i1 = e1 && i2 = e2 then Add(i1,i2)
            else simplify (Add(i1,i2))
        | Sub(e1, e2) -> 
            let i1 = simplify e1
            let i2 = simplify e2
            if i1 = e1 && i2 = e2 then Sub(i1,i2)
            else simplify (Sub(i1,i2))
        | Mul(e1, e2) -> 
            let i1 = simplify e1
            let i2 = simplify e2
            if i1 = e1 && i2 = e2 then Mul(i1,i2)
            else simplify (Mul(i1,i2))
        | _ -> aexpr //No simplification can be done.

    let example4 = Add(Var "x", CstI 0)
    let simplified4 = simplify example4
    // simplified1 should be Var "x"

    let example5 = Mul(Add(Var "x", CstI 0), CstI 1)
    let simplified5 = simplify example5
    // simplified2 should be Var "x"

    let example6 = Mul(Add(CstI 1, CstI 0), Sub(Var "y", CstI 0))
    let simplified6 = simplify example6
    // simplified3 should be Var "y"       


    // 1.2.5

    




    // 1.3
    let rec fmt2 pre aexpr: string =
        let currentPrec= 
            match aexpr with
            | Add _  -> 2
            | Sub _ -> 2
            | Mul _ -> 3
            | _ -> 4
        let formatted = 
            match aexpr with
            | CstI i -> string i
            | Var x -> x
            | Add (e1, e2) -> fmt2 2 e1 + " + " + fmt2 3 e2
            | Sub (e1, e2) -> fmt2 2 e1 + " - " + fmt2 3 e2
            | Mul (e1, e2) -> fmt2 3 e1 + " * " + fmt2 4 e2

        if currentPrec < pre then
            "(" + formatted + ")"
        else
            formatted
    let example7 = Mul(Sub(Var "a", Var "b"), Var "c")
    let removeExcessParenth7 = fmt2 0 example7    
    // removeExessParenth7 should be "(a-b)*c"

    let example8 = Sub(Mul(Var "a", Var "b"), Var "c")
    let removeExcessParenth8 = fmt2 0 example8   
    // removeExessParenth8 should be "a*b-c"

    let example9 = Sub(Sub(Var "a", Var "b"), Var "c")
    let removeExcessParenth9 = fmt2 0 example7    
    // removeExessParenth9 should be "a-b-c"

    let example10 = Sub(Var "a", Sub(Var "b", Var "c")) 
    let removeExcessParenth10 = fmt2 0 example7  
    // removeExessParenth10 should be "a-(b-c)"   
