(* File Expr/Parse.fs *)
(* Lexing and parsing of simple expressions using fslex and fsyacc *)

module Parse

open System
open System.IO
open System.Text
// open Microsoft.FSharp.Text.Lexing
open Absyn

(* Plain parsing from a string, with poor error reporting *)

let fromString (str: string) : expr =
    let lexbuf = FSharp.Text.Lexing.LexBuffer<char>.FromString(str)

    try
        ExprPar.Main ExprLex.Token lexbuf
    with exn ->
        let pos = lexbuf.EndPos
        failwithf "%s near line %d, column %d\n" (exn.Message) (pos.Line + 1) pos.Column

(* Parsing from a text file *)

let fromFile (filename: string) =
    use reader = new StreamReader(filename)
    let lexbuf = FSharp.Text.Lexing.LexBuffer<char>.FromTextReader reader

    try
        ExprPar.Main ExprLex.Token lexbuf
    with exn ->
        let pos = lexbuf.EndPos
        failwithf "%s in file %s near line %d, column %d\n" (exn.Message) filename (pos.Line + 1) pos.Column
(*
let compString (input: string) : sinstr list =
    let parsedExpr = fromString input
    let compiledInstructions = scomp parsedExpr []

    compiledInstructions
*)
// val it: sinstr list = [SCstI 1; SCstI 1; SAdd]
