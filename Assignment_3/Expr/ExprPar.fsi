// Signature file for parser generated by fsyacc
module ExprPar
type token = 
  | EOF
  | IF
  | THEN
  | ELSE
  | QMARK
  | COLON
  | LPAR
  | RPAR
  | END
  | IN
  | LET
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EQ
  | NAME of (string)
  | CSTINT of (int)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_IF
    | TOKEN_THEN
    | TOKEN_ELSE
    | TOKEN_QMARK
    | TOKEN_COLON
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_END
    | TOKEN_IN
    | TOKEN_LET
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_TIMES
    | TOKEN_DIVIDE
    | TOKEN_EQ
    | TOKEN_NAME
    | TOKEN_CSTINT
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startMain
    | NONTERM_Main
    | NONTERM_Expr
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val Main : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (Absyn.expr) 
