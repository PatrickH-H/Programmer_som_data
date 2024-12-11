
module Kommatal_fslex
open FSharp.Text.Lexing
open System/// Rule tokenize
val tokenize: lexbuf: LexBuffer<char> -> token
