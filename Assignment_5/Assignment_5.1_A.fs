(*Exercise 5.1 A*)

let rec merge (xs: int list, ys: int list) : int list =
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x :: xs', y :: ys' -> if x < y then x :: merge (xs', ys) else y :: merge (xs, ys')

(*Exercise 5.7*)

//???

(*Exercise 6.1*)

(*Exercise 6.2*)

(*Exercise 6.3*)

(*Exercise 6.4*)

(*Exercise 6.5*)
