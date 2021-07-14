{
open Token
open Lexing
open Printf
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let comment = '#' [^ '\n']*

let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z']

let ident = alpha (alpha | digit)*
let int   = digit+
let float = (digit+ '.' digit*)

rule token = parse
    (* ignore *)
    | white         { token lexbuf }
    | comment       { token lexbuf }
    | newline       { new_line lexbuf; token lexbuf }
    (* values *)
    | "null"        { NULL }
    | "true"        { TRUE }
    | "false"       { FALSE }
    (* keywords *)
    | "function"    { FUNCTION }
    | "var"         { VAR }
    | "if"          { IF }
    | "then"        { THEN }
    | "else"        { ELSE }
    (* logical operations *)
    | "||"          { OR }
    | "&&"          { AND }
    (* comparisons *)
    | "=="          { EQEQ }
    | "!="          { NE }
    | '<'           { LT }
    | "<="          { LE }
    | '>'           { GT }
    | ">="          { GE }
    (* operations *)
    | '+'           { PLUS }
    | '-'           { MIN }
    | '*'           { MULT }
    | '/'           { DIV }
    | '%'           { MOD }
    | '!'           { NOT }
    (* symbols *)
    | '('           { LPAREN }
    | ')'           { RPAREN }
    | '{'           { LBRACE }
    | '}'           { RBRACE }
    | ';'           { SEMICOLON }
    | ','           { COMMA }
    | '.'           { DOT }
    | '='           { EQ }
    (* variables (has to be at the end to avoid seeing keywords as identifiers) *)
    | ident         { IDENT (lexeme lexbuf) }
    | int           { INT (int_of_string @@ lexeme lexbuf) }
    | float         { FLOAT (float_of_string @@ lexeme lexbuf) }
    (* internal *)
    | eof           { EOF }
    | _             { lexer_err @@ sprintf "unexpected symbol `%s'" (lexeme lexbuf) }
