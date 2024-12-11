(* 
Lavet af Marcus Kofoed Kirkegaard (mkki@itu.dk)
"Jeg erklærer hermed at jeg selv har lavet hele denne eksamensbesvarelse uden hjælp fra andre"


Opgave 1 - Regulære udtryk og automater (repo/Lecu2/helloLex)

    1) Answer:
        Det regulære udtryk "d+ ′,′? d*" repræsenterer alle tal mellem 0 og 9 fra 1 til "n"-gange enten alene 
        eller efterfulgt af et komma, og tal mellem 0 og 9 fra 0 til "n" gange.

        Sproget beskriver numeriske strings og kan repræsenterer.. 
        * integers
        * decimal tal med et komma som decimal seperator

        (d+ -> et digit 1 eller flere gange)
        (´,´? -> enten eller, altså enten komma eller ikke komma)
        (d* -> et digit 0 eller flere gange)
        
        Eksempler:
        4
        1,01
        0,
        9,1234567


    2) Answer:
        a) Ja, tilstandsmaskinen acceptere netop de strenge, som genkendes af det regulære udtryk ovenfor. 
        
        b) Tilstandsmaskinen er NFA, eftersom den har epsilon skift til 3, 4 og 7. Den kan derfor ikke være DFA. 

    3) Answer: 
        (d+ (´,´ d+)?)?

    4) Answer:
        run fslex -o <nameOfNewFile> file.fs
        

Opgave 2 - Icon (Lectures/Lec11/Cont)
    1) Answer:
        *) let icon1 = Every(Write(FromTo(1,6))) (*
        Ved at bruge icon.fs.. somethinhg something

    2) Answer:
        *) let icon2 = Every(Write(And(FromTo(3,6), FromTo(3,6)))) (*

    3) Answer:
        *)
        type expr += 
        | FromToBy of int * int * int

        let rec eval (e : expr) (cont : cont) (econt : econt) =
            match e with
            | FromToBy(s, e, i) when s <= e && i >= 0 ->
                let rec loop curr =
                    if curr <= e then
                        cont (Int curr) (fun () -> loop (curr + i))
                    else
                        econt ()
                loop s
            | FromToBy _ -> econt ()
        (*

    4) Answer:
        *) let icon4 = Every(Write(And(FromToBy(3,6,1), FromToBy(3,6,1)))) (*

    5) Answer:
        Afhænger af hvordan koden er skrevet
        *) let icon5 = Every(Write(FromToBy(10,10,0))) (*



Opgave 3 - Print i micro-ML (Lectures/Lec05/Fun)
    1) Answer:
        *)
        type expr = 
            | Print of expr
        (*
    
    2) Answer:
        *)
        rule Token = parse
            | "print"    { PRINT }

        // In parser (FunPar.fsy)
        %token PRINT

        // Grammar rule
        Expr: 
            | PRINT Expr   { Print($2) }
        (*

        Examples:

        *)
        // ex1: print 1
        let ex1 = Print(CstI 1)
        // ex2: print ((print 1) + 3)
        let ex2 = Print(Prim("+", Print(CstI 1), CstI 3))
        // ex3: let f x = x + 1 in print f end
        let ex3 = Let("f", Prim("+", Var "x", CstI 1), Print(Var "f"))

        let ex4 = Print(Call("someFunc", [CstI 42]))
        let ex5 = Let("x", CstI 10, Print(Var "x"))
        let ex6 = Print(Prim("*", CstI 6, CstI 7))    
        (*
    
    3) Answer:
        *)
        let rec eval (env: env) (e: expr) : value =
            match e with
            | Print(body) ->
                let v = eval env body
                printfn "%A" v  
                v
        (*

    


Opgave 4 - Tupler i List-C (Lectures/Lec10/ListC)
    1) Answer: 
        Absyn.fs:
            *)
            type expr = 
                | PrimN of string * expr list 
            (*
        CLex.fsl:
            *)
            rule Token = parse
                | "tup"    { TUP }
                | "upd"    { UPD }
                | "nth"    { NTH }
            (*    
        CPar.fsy:
            *)
            %token TUP UPD NTH

            Expr:
                | TUP '(' ExprList ')' { PrimN("tup", $3) }
                | UPD '(' Expr ',' Expr ',' Expr ')' { PrimN("upd", [$3; $5; $7]) }
                | NTH '(' Expr ',' Expr ')' { PrimN("nth", [$3; $5]) }
            (*
        Machine.fs:
            *)
            type instruction = 
                | TUP of int   
                | UPD          
                | NTH          
            (*
        Comp.fs:
            *)
            let rec cExpr e cont =
                match e with
                | PrimN("tup", es) ->
                    cExprs es cont @ 
                    [TUP(List.length es)]
                | PrimN("upd", [t; CstI i; v]) ->
                    cExpr t cont @
                    cExpr v cont @
                    [UPD]
                | PrimN("nth", [t; CstI i]) ->
                    cExpr t cont @
                    [NTH]
            (*
        listmachine.fs
            *)
            case 32: 
                n = Pop();
                p = allocate(n+1); 
                Mem[p] = 1;  
                for (i = n; i > 0; i--) {
                    Mem[p+i] = Pop();
                }
                Push(p);
                break;

            case 33: 
                v = Pop();
                i = Untag(Pop());
                p = Pop();
                Mem[p+i+1] = v;
                Push(p);
                break;

            case 34: 
                i = Untag(Pop());
                p = Pop();
                Push(Mem[p+i+1]);
                break;
            (*
    
    2) Answer:
        testProgram.fs:
            *)
            void main() {
                dynamic t1;
                t1 = tup(32,33,34);
                printTuple(t1,3); // Should print: 32 33 34
                upd(t1,0,42);
                printTuple(t1,3); // Should print: 42 33 34
                dynamic t2;
                t2 = tup(10,11,12,13,14,15);
                upd(t2,5,42);
                printTuple(t2,6); // Should print: 10 11 12 13 14 42
            }
            (*
*)
