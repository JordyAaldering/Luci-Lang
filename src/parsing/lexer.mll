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
    (* keywords *)
    | "class"       { CLASS }
    | "function"    { FUNCTION }
    | "var"         { VAR }
    | "if"          { IF }
    | "then"        { THEN }
    | "else"        { ELSE }
    | "while"       { WHILE }
    | "for"         { FOR }
    | "do"          { DO }
    | "return"      { RETURN }
    | "print"       { PRINT }
    (* operations *)
    | "||"          { OR }
    | "&&"          { AND }
    | "=="          { EQEQ }
    | "!="          { NE }
    | '<'           { LT }
    | "<="          { LE }
    | '>'           { GT }
    | ">="          { GE }
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
    (* primary *)
    | "null"        { NULL }
    | "true"        { TRUE }
    | "false"       { FALSE }
    | "this"        { THIS }
    | "super"       { SUPER }
    | int           { INT (int_of_string @@ lexeme lexbuf) }
    | float         { FLOAT (float_of_string @@ lexeme lexbuf) }
    | ident         { IDENT (lexeme lexbuf) }
    | eof           { EOF }
    | _             { parse_err @@ sprintf "unexpected symbol `%s'" (lexeme lexbuf) }
