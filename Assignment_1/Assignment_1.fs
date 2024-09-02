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
