// Exercise 2.4


type expr = 
  | CstI of int
  | Var of string
  | Let of string * expr * expr
  | Prim of string * expr * expr

type stackvalue =
  | Value                            
  | Bound of string                 

type sinstr = 
    | SCstI of int
    | SVar of int
    | SAdd
    | SSub
    | SMul
    | SPop
    | SSwap


let getindex (cenv: stackvalue list) (bound: stackvalue) : int =
    List.findIndex ((=) bound) cenv 


let rec scomp (e : expr) (cenv : stackvalue list) : sinstr list =
    match e with
    | CstI i -> [SCstI i]
    | Var x  -> [SVar (getindex cenv (Bound x))]
    | Let(x, erhs, ebody) -> 
          scomp erhs cenv @ scomp ebody (Bound x :: cenv) @ [SSwap; SPop]
    | Prim("+", e1, e2) -> 
          scomp e1 cenv @ scomp e2 (Value :: cenv) @ [SAdd] 
    | Prim("-", e1, e2) -> 
          scomp e1 cenv @ scomp e2 (Value :: cenv) @ [SSub] 
    | Prim("*", e1, e2) -> 
          scomp e1 cenv @ scomp e2 (Value :: cenv) @ [SMul] 
    | Prim _ -> failwith "scomp: unknown operator"




let sinstrToInt (instr: sinstr) : int list = 
    match instr with
    | SCstI x -> [0; x]
    | SVar x -> [1; x]
    | SAdd -> [2]
    | SSub -> [3]
    | SMul -> [4]
    | SPop -> [5]
    | SSwap -> [6]      

let assemble (instructions: sinstr list) : int list =
    instructions |> List.collect sinstrToInt

let compile (expr: expr) : int list =
    let instructions = scomp expr [] 
    assemble instructions             
    
let expr1 = CstI 42
let expr2 = Prim("+", CstI 10, CstI 20)
let expr3 = Prim("*", Prim("+", CstI 3, CstI 4), CstI 2)
let expr4 = Let("x", CstI 5, Prim("+", Var "x", CstI 10))

let test1 = compile expr1  
// test1 should be: [0; 42]
let test2 = compile expr2  
// test2 should be: [0; 10; 0; 20; 2]
let test3 = compile expr3  
// test3 should be: [0; 3; 0; 4; 2; 0; 2; 4]
let test4 = compile expr4  
// test4 should be: [0; 5; 1; 0; 0; 10; 2; 6; 5]

// Printing results
printfn "Test 1 (CstI 42): %A" test1
printfn "Test 2 (Prim +): %A" test2
printfn "Test 3 (Nested Prim): %A" test3
printfn "Test 4 (Let and Prim +): %A" test4


let intsToFile (inss : int list) (fname : string) = 
    let text = String.concat " " (List.map string inss)
    System.IO.File.WriteAllText(fname, text);;


intsToFile(compile expr4) "test4.txt"


// Exercise 3.2