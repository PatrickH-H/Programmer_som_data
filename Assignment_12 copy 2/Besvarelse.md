Chapter 13: 13.1 (1 point), 13.2 (2 points), 13.3 (1 point, see slides for 13.3). A total for 4 points can be earned this week.

# 13.1

(Kan ikke køre de filer på mac ordentligt... bruger noget der hedder wine, men så kan den ikke finde fslexyacc runtime)

1. Result of running ex09.out
   (Kan ikke generere den?)
   TODO

2. Typen af resultatet værdien
   TODO

3. What application calls have been annotated as tail calls? Explain how this matches
   the intuition behind a tail call
   TODO

4. What type has been annotated for the call sites to the functions f and g? Function
   f is called in two places, and g in one place.
   TODO

5. What is the running time for executing the example using the evaluator, and what
   is the running time using the byte code ex09.out using msmlmachine?
   TODO

6. Now compile the example ex09.sml without optimizations. How many byte
   code instructions did the optimization save for this small example?
   TODO

# 13.2

1.  fst(p: A _ B): A  
    (if p : t1 _ t2, then f st(p) : t1)

snd(p: A _ B): B
(if p : t1 _ t2, then snd(p) : t2)

2.  fst(PairV(v1, v2)) -> v1
    eval(fst(p)) = v1 where p = (v1, v2)

snd(PairV(v1, v2)) -> v2
eval(snd(p)) = v2 where p = (v1, v2)

3. FunLex.fsl
   | "," { COMMA }
   | "fst" { FST }
   | "snd" { SND }

4. FunPar.fsy
   Expr:
   | "(" Expr COMMA Expr ")" { Pair($2, $4, None) }
   | "fst" "(" Expr ")" { Fst($3, None) }
   | "snd" "(" Expr ")" { Snd($3, None) }

5. Absyn.fs
   type expr<'a> =
   | Pair of expr<'a> _ expr<'a> _ 'a option
   | Fst of expr<'a> _ 'a option
   | Snd of expr<'a> _ 'a option
6. TypeInference.fs
   type typ =
   | TypP of typ \* typ

let rec typExpr e tenv =
match e with
| Fst(e1, _) ->
let t = typExpr e1 tenv
(match t with
| TypP(t1, _) -> t1
| _ -> failwith "fst applied to non-pair")
| Snd(e1, _) ->
let t = typExpr e1 tenv
(match t with
| TypP(_, t2) -> t2
| _ -> failwith "snd applied to non-pair")
| Pair(e1, e2, \_) ->
let t1 = typExpr e1 tenv
let t2 = typExpr e2 tenv
TypP(t1, t2)

7. HigherFun.fs
   type value =
   | PairV of value \* value

let rec evalExpr env expr =
match expr with
| Fst(e1, _) ->
(match evalExpr env e1 with
| PairV(v1, _) -> v1
| _ -> failwith "fst applied to non-pair")
| Snd(e1, _) ->
(match evalExpr env e1 with
| PairV(_, v2) -> v2
| _ -> failwith "snd applied to non-pair")
| Pair(e1, e2, \_) ->
let v1 = evalExpr env e1
let v2 = evalExpr env e2
PairV(v1, v2)

8. msmlmachine.c

case PAIR: {
int v2 = pop();
int v1 = pop();
int pair = heap_alloc_pair(v1, v2);
push(pair);
break;
}
case PRINTP: {
int pair = pop();
printf("(%d,%d)\n", heap_fst(pair), heap_snd(pair));
push(pair);
break;
}

9. ContComp.fs
   | Fst(e1, _) ->
   cExpr e1 lab env @ [CAR]
   | Snd(e1, _) ->
   cExpr e1 lab env @ [CDR]
   | Pair(e1, e2, \_) ->
   cExpr e1 lab env @ cExpr e2 lab env @ [PAIR]

10. Machine.fs
    | PAIR ->
    let v2 = stack.Pop()
    let v1 = stack.Pop()
    let pair = heap.AllocPair(v1, v2)
    stack.Push(pair)
    | PRINTP ->
    let pair = stack.Pop()
    let (v1, v2) = heap.GetPair(pair)
    printfn "(%d, %d)" v1 v2
    stack.Push(pair)

# 13.3 Alpha conversion

(ikke helt sikker om det er på de downloadede filer?)

type expr =
| Var of string  
 | Fun of string _ expr  
 | Let of string _ expr _ expr  
 | App of expr _ expr  
 | Other

type Env = Map<string, string>

let rec alphaConvert (expr: expr) (env: Env) (counter: int): expr \* int =
match expr with
| Var(name) ->
let newName = Map.tryFind name env |> Option.defaultValue name
Var(newName), counter
| Fun(param, body) ->
let newParam = param + "_" + string counter
let newEnv = Map.add param newParam env
let newBody, nextCounter = alphaConvert body newEnv (counter + 1)
Fun(newParam, newBody), nextCounter
| Let(name, value, body) ->
let newName = name + "_" + string counter
let newEnv = Map.add name newName env
let newValue, nextCounter1 = alphaConvert value env (counter + 1)
let newBody, nextCounter2 = alphaConvert body newEnv nextCounter1
Let(newName, newValue, newBody), nextCounter2
| App(func, arg) ->
let newFunc, nextCounter1 = alphaConvert func env counter
let newArg, nextCounter2 = alphaConvert arg env nextCounter1
App(newFunc, newArg), nextCounter2
| Other ->
expr, counter

let performAlphaConversion (expr: expr): expr =
let initialEnv = Map.empty
let renamedExpr, \_ = alphaConvert expr initialEnv 0
renamedExpr

// Testing / Example:
//Input
//Let("x", Var("y"), Fun("x", App(Var("x"), Var("y"))))

//output
//Let("x_0", Var("y"), Fun("x_1", App(Var("x_1"), Var("y"))))
