// Exercise 2.4

type sinstr = 
    | SCst of int
    | SVar of int
    | SAdd
    | SSub
    | SMul
    | SPop
    | SSwap



let sinstrToInt (instr: sinstr) : int list = 
    match instr with
    | SCst x -> [0; x]
    | SVar x -> [1; x]
    | SAdd -> [2]
    | SSub -> [3]
    | SMul -> [4]
    | SPop -> [5]
    | SSwap -> [6]