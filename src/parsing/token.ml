open Ast
open Printf

exception Lexer_error of string

let lexer_err msg =
    raise @@ Lexer_error msg

type token =
    (* primary *)
    | NULL
    | TRUE
    | FALSE
    | INT of int
    | FLOAT of float
    | IDENT of string
    (* keywords *)
    | FUNCTION
    | VAR
    | IF
    | THEN
    | ELSE
    | RETURN
    | PRINT
    (* operations *)
    | OR
    | AND
    | EQEQ
    | NE
    | LT
    | LE
    | GT
    | GE
    | PLUS
    | MIN
    | MULT
    | DIV
    | MOD
    | NOT
    (* symbols *)
    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    | SEMICOLON
    | COMMA
    | DOT
    | EQ
    | EOF

(**
 * A module containing helper methods for tokens.
 *)
module Token = struct

let to_str token =
    match token with
    (* primary *)
    | NULL -> "null"
    | TRUE -> "true"
    | FALSE -> "false"
    | INT x -> string_of_int x
    | FLOAT x -> string_of_float x
    | IDENT s -> s
    (* keywords *)
    | FUNCTION -> "function"
    | VAR -> "var"
    | IF -> "if"
    | THEN -> "then"
    | ELSE -> "else"
    | RETURN -> "return"
    | PRINT -> "print"
    (* oparation *)
    | OR -> "||"
    | AND -> "&&"
    | EQ -> "="
    | EQEQ -> "=="
    | NE -> "!="
    | LT -> "<"
    | LE -> "<="
    | GT -> ">"
    | GE -> ">="
    | PLUS -> "+"
    | MIN -> "-"
    | MULT -> "*"
    | DIV -> "/"
    | MOD -> "%"
    | NOT -> "!"
    (* symbols *)
    | LPAREN -> "("
    | RPAREN -> ")"
    | LBRACE -> "{"
    | RBRACE -> "}"
    | SEMICOLON -> ";"
    | COMMA -> ","
    | DOT -> "."
    | EOF -> "EOF"

let is_bop token =
    match token with
    | OR
    | AND
    | EQEQ
    | NE
    | LT
    | LE
    | GT
    | GE
    | PLUS
    | MIN
    | MULT
    | DIV
    | MOD
    | NOT -> true
    | _ -> false

let is_uop token =
    match token with
    | MIN
    | NOT -> true
    | _ -> false

let to_bop token =
    match token with
    | OR -> OpOr
    | AND -> OpAnd
    | EQEQ -> OpEq
    | NE -> OpNe
    | LT -> OpLt
    | LE -> OpLe
    | GT -> OpGt
    | GE -> OpGe
    | PLUS -> OpPlus
    | MIN -> OpMin
    | MULT -> OpMult
    | DIV -> OpDiv
    | MOD -> OpMod
    | _ -> lexer_err @@ sprintf "expected a binary operator, got `%s'" (to_str token)

let to_uop token =
    match token with
    | MIN -> OpNeg
    | NOT -> OpNot
    | _ -> lexer_err @@ sprintf "expected a unary operator, got `%s'" (to_str token)

let precedence token =
    match token with
    | OR -> 1
    | AND -> 2
    | EQEQ
    | NE -> 3
    | LT
    | LE
    | GT
    | GE -> 4
    | PLUS
    | MIN -> 5
    | MULT
    | DIV
    | MOD -> 6
    | _ -> lexer_err @@ sprintf "expected a binary operator, got `%s'" (to_str token)

end (* Token *)
