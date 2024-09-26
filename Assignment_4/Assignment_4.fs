(*EXERCISE 4.1*)

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

(*EXERCISE 4.4*)

(*EXERCISE 4.5*)
