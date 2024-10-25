(*EXERCISE 4.1*)

// How should we provide this, it runs fine? :D

(*EXERCISE 4.2*)

//A)
let rec sum n = if n <= 1 then 1 else n + sum (n - 1)

let result = sum 1000

//B)
let rec pow a b = if b = 0 then 1 else a * pow a (b - 1)

let result = pow 3 8

//C)
let rec sum_of_powers b from_exp to_exp =
    if from_exp > to_exp then
        0
    else
        pow b from_exp + sum_of_powers b (from_exp + 1) to_exp

let result = sum_of_powers 3 0 11

//D)
let rec sum_of_eighth_powers from_base to_base exp =
    if from_base > to_base then
        0
    else
        pow from_base exp + sum_of_eighth_powers (from_base + 1) to_base exp

let result = sum_of_eighth_powers 1 10 8

(*EXERCISE 4.3*)
(*
    Added to Absyn.fs:
        expr:
            | Letfun of string list * string * expr * expr 
            | Call of expr * expr list 
    Added to Fun.fs:
        | Letfun(p, x, fBody, letBody) -> 
            let closure = Closure(p, x, fBody, env)
            let bodyEnv = (List.head p, closure) :: env
            eval letBody bodyEnv
        | Call(Var f, eArgs) -> 
            let fClosure = lookup env f

            match fClosure with
            | Closure(p, _, fBody, fDeclEnv) ->
                let argValues = List.map (fun arg -> eval arg env) eArgs

                let fBodyEnv =
                    List.zip p argValues
                    |> List.fold (fun env (param, value) -> (param, value) :: env) fDeclEnv

                eval fBody fBodyEnv
            | _ -> failwith "eval Call: not a function"
            | Call _ -> failwith "eval Call: not first-order function"

                
        let ex6 = 
            Letfun([ "add"; "b" ], "a", Prim("+", Var "a", Var "b"), Call(Var "add", [ CstI 3; CstI 5 ]))
    
*)

(*EXERCISE 4.4*)
(*
    Added to FunPar.fsy:
        %type <string list> Params
        %type <Absyn.epxr list> Args

        AppExpr:
            AtExpr Args                         { Call($1, $2)           }
        ;

        Params: 
            Name                                { [$1]                   }
        | Name Params                         { $1 :: $2               }
        ;

        Args: 
                                                { []                     }
        | Expr                                { [$1]                   }
        | Expr Args                           { $1 :: $2               }


        (Changes to:
            | LET Params NAME EQ Expr IN Expr END      { Letfun($2, $3, $5, $7) }
        )

    Test in "dotnet fsi":
    input:      Call(Var "f", [Var "a"; Var "b"]);;
    output:     val it: expr = Call (Var "f", [Var "a"; Var "b"])
*)

(*EXERCISE 4.5*)
(*
    Added to FunPar.fsy:
        %token AND OR
        %nonassoc AND
        %nonassoc OR

        %type <string list> Params
        %type <Absyn.expr list> Args 
        | Expr AND Expr                       { Prim("&&", $1, $3)     }
        | Expr OR Expr                        { Prim("||", $1, $3)     }

    Added to FunLex.fsl:
        | "&&"            { AND }
        | "||"            { OR }

    Test in dotnet fsi: 
        input: true && false;;
        output: val it: bool = false

        input: true || false;;
        output: val it: bool = true

*)
