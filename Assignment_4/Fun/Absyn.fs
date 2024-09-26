(* Fun/Absyn.fs * Abstract syntax for micro-ML, a functional language *)

module Absyn

type expr =
    | CstI of int
    | CstB of bool
    | Var of string
    | Let of string * expr * expr
    | Prim of string * expr * expr
    | If of expr * expr * expr
    | Letfun of string list * string * expr * expr // Our changed code to include a list of parameter names
    | Call of expr * expr list // Our changed code to include a list of argument expressions
