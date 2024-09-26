// Exercise 3.3
//
// Write out the right most derivation of the string below from the expression grammar
// at the end of Sect. 3.6.5, corresponding to ExprPar.fsy.
// Take note of the sequence of grammar rules (A–I) used.
// let z = (17) in z + 2 * 3 end EOF


(* Answer:

    1) Main
    2) Expr EOF (Rule A)
    3) LET NAME EQ Expr IN Expr END EOF (Rule F)
    4) LET NAME EQ Expr IN Expr PLUS Expr END EOF (Rule H)
    5) LET NAME EQ Expr IN Expr PLUS Expr TIMES Expr END EOF (Rule G)
    6) LET NAME EQ Expr IN Expr PLUS Expr TIMES 3 END EOF (Rule C)
    7) LET NAME EQ Expr IN Expr PLUS 2 TIMES 3 END EOF (Rule C)
    8) LET NAME EQ Expr IN z PLUS 2 TIMES 3 END EOF (Rule B)
    9) LET NAME EQ LPAR Expr RPAR IN z PLUS 2 TIMES 3 END EOF (Rule E)
    10) LET NAME EQ LPAR 17 RPAR IN z PLUS 2 TIMES 3 END EOF (Rule C)
    11) let z = (17) in z + 2 * 3 end EOF (Rule B)

*)


// Exercise 3.4
// Draw the above derivation as a tree.
(* Answer:

                                Main
                                  |
                              Expr  EOF
                                |
                LET NAME EQ Expr IN Expr END
                  |           |        |
                  z  LPAR Expr RPAR   Expr PLUS Expr
                            |           |        |
                          CSTINT      Expr TIMES Expr
                            |         |     |      |
                           17         z     2      3
*)

// Exercise 3.5
// Get expr.zip from the book homepage and unpack it. Using a command prompt, generate (1) the lexer and (2) the parser for expressions by running fslex and fsyacc; then (3) load the expression abstract syntax, the lexer and parser modules, and the expression interpreter and compilers, into an interactive F# session (fsharpi):
//   fslex --unicode ExprLex.fsl
//   fsyacc --module ExprPar ExprPar.fsy
//   fsharpi -r FSharp.PowerPack.dll Absyn.fs ExprPar.fs ExprLex.fs ˆ
// Parse.fs
// Now try the parser on several example expressions, both well-formed and ill-formed ones, such as these, and some of your own invention:
//   open Parse;;
//   fromString "1 + 2 * 3";;
//   fromString "1 - 2 - 3";;
//   fromString "1 + -2";;
//   fromString "x++";;
//   fromString "1 + 1.2";;
//   fromString "1 + ";;
//   fromString "let z = (17) in z + 2 * 3 end";;
//   fromString "let z = 17) in z + 2 * 3 end";;
//   fromString "let in = (17) in z + 2 * 3 end";;
//   fromString "1 + let x=5 in let y=7+x in y+y end + x end";;

(* Answer:
  How is it inteded that we "document"/answer this (3.5)?
*)

// Exercise 3.6
// Use the expression parser from Parse.fs and the compiler scomp (from expressions to stack machine instructions)
// and the associated datatypes from Expr.fs, to define a function
//     compString : string -> sinstr list
// that parses a string as an expression and compiles it to stack machine code.

(* Answer:
  We did not have enough time, will do it for resubmission
*)

// Exercise 3.7
// Extend the expression language abstract syntax and the lexer and parser specifications with conditional expressions. The abstract syntax should be If(e1, e2, e3),somodifyfileAbsyn.fsaswellasExprLex.fslandfile ExprPar.fsy. The concrete syntax may be the keyword-laden F#/ML-style:
//   if e1 then e2 else e3
// or the more light-weight C/C++/Java/C#-style:
// e1 ? e2 : e3
// Some documentation for fslex and fsyacc is found in this chapter and in Expert
// F# [17].

(* Answer:
  We did not have enough time, will do it for resubmission
*)
