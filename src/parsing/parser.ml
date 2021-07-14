open Ast
open Token
open Lexer
open Printf

exception Parse_error of string

let parse_err msg =
    raise @@ Parse_error msg

module TokenStack = struct
    let stack = ref []

    let get lexbuf =
        match !stack with
        | [] -> token lexbuf
        | h :: t -> stack := t; h

    let unget token =
        stack := token :: !stack

    let peek lexbuf =
        let t = get lexbuf in
        unget t;
        t
    
    let peek_snd lexbuf =
        let t1 = get lexbuf in
        let t2 = get lexbuf in
        unget t2;
        unget t1;
        t2

    let match_no_consume lexbuf expected =
        let t = peek lexbuf in
        if t = expected then true
        else false

    let match_and_consume lexbuf expected =
        let t = get lexbuf in
        if t = expected then true
        else (unget t; false)

    let assert_and_consume lexbuf expected =
        let t = get lexbuf in
        if t <> expected then
            parse_err @@ sprintf "expected token `%s', got `%s'"
                (Token.to_str expected) (Token.to_str t);
        ()
end (* TokenStack *)

let assert_and_get_identifier lexbuf =
    let t = TokenStack.get lexbuf in
    match t with
    | IDENT s -> s
    | _ -> parse_err @@ sprintf "expected an identifier, got `%s'" (Token.to_str t)

(**
 * Program
 *)

let rec parse_program lexbuf =
    let declarations = ref [] in
    while not (TokenStack.match_and_consume lexbuf EOF) do
        declarations := !declarations @ [parse_declaration lexbuf]
    done;
    !declarations

(**
 * Declarations
 *)

and parse_declaration lexbuf =
    let t = TokenStack.peek lexbuf in
    match t with
    | FUNCTION -> parse_function_decl lexbuf
    | VAR -> parse_var_decl lexbuf
    | _ -> parse_statement lexbuf

and parse_function_decl lexbuf =
    TokenStack.assert_and_consume lexbuf FUNCTION;
    let name = assert_and_get_identifier lexbuf in

    TokenStack.assert_and_consume lexbuf LPAREN;
    let args = if not (TokenStack.match_and_consume lexbuf RPAREN) then
            let args = parse_identifiers lexbuf in
            TokenStack.assert_and_consume lexbuf RPAREN;
            args
        else []
    in
    let block = parse_block lexbuf in
    EFunc (name, args, block)

and parse_var_decl lexbuf =
    TokenStack.assert_and_consume lexbuf VAR;
    let name = assert_and_get_identifier lexbuf in

    let e = if TokenStack.match_and_consume lexbuf EQ then
            parse_expr lexbuf
        else ENull
    in
    TokenStack.assert_and_consume lexbuf SEMICOLON;
    EVar (name, e)

(**
 * Statements
 *)

and parse_statement lexbuf =
    let t = TokenStack.peek lexbuf in
    match t with
    | IF -> parse_cond_stmt lexbuf
    | LPAREN -> parse_block lexbuf
    | _ -> parse_expr_stmt lexbuf

and parse_cond_stmt lexbuf =
    TokenStack.assert_and_consume lexbuf IF;
    let e1 = parse_expr lexbuf in

    TokenStack.assert_and_consume lexbuf THEN;
    let e2 = parse_statement lexbuf in
    
    let e3 = if TokenStack.match_and_consume lexbuf ELSE then
            parse_statement lexbuf
        else ENull
    in
    ECond (e1, e2, e3)

and parse_expr_stmt lexbuf =
    let e = parse_expr lexbuf in
    TokenStack.assert_and_consume lexbuf SEMICOLON;
    e

(**
 * Expressions
 *)

and parse_expr lexbuf =
    parse_assignment lexbuf

and parse_assignment lexbuf =
    let t1 = TokenStack.peek lexbuf in
    let t2 = TokenStack.peek_snd lexbuf in
    match t1, t2 with
    | VAR, _ ->
        TokenStack.assert_and_consume lexbuf VAR;
        let s = assert_and_get_identifier lexbuf in
        let e = if TokenStack.match_and_consume lexbuf EQ then
                parse_assignment lexbuf
            else ENull
        in
        EVar (s, e)
    | IDENT _s, EQ ->
        let s = assert_and_get_identifier lexbuf in
        TokenStack.assert_and_consume lexbuf EQ;
        let e = parse_assignment lexbuf in
        EAssign (s, e)
    | _ -> parse_binary lexbuf

and parse_binary lexbuf =
    let rec resolve_stack s prec =
        let e1, op1, prec1 = Stack.pop s in
        if prec <= prec1 then (
            let e2, op2, prec2 = Stack.pop s in
            let e = EBinary (Token.to_bop op1, e2, e1) in
            Stack.push (e, op2, prec2) s;
            resolve_stack s prec
        ) else Stack.push (e1, op1, prec1) s
    in

    let e1 = parse_unary lexbuf in
    let s = Stack.create () in
    Stack.push (e1, EOF, -1) s;
    while Token.is_bop @@ TokenStack.peek lexbuf do
        let t = TokenStack.get lexbuf in
        resolve_stack s (Token.precedence t);
        let e2 = parse_unary lexbuf in
        Stack.push (e2, t, Token.precedence t) s;
    done;
    resolve_stack s 0;
    let e, _op, _prec = Stack.pop s in
    e

and parse_unary lexbuf =
    if List.exists (TokenStack.match_no_consume lexbuf) [NOT; MIN] then
        let op = Token.to_uop (TokenStack.get lexbuf) in
        let e = parse_unary lexbuf in
        EUnary (op, e)
    else parse_call lexbuf

and parse_call lexbuf =
    let e = parse_primary lexbuf in
    let t = TokenStack.peek lexbuf in
    match t with
    | LPAREN -> begin
        TokenStack.assert_and_consume lexbuf LPAREN;
        let args = parse_arguments lexbuf in
        TokenStack.assert_and_consume lexbuf RPAREN;
        ECall (e, args)
    end
    | DOT -> begin
        TokenStack.assert_and_consume lexbuf DOT;
        let ids = parse_identifiers ~delim:DOT lexbuf in
        ESelect (e, ids)
    end
    | _ -> e

and parse_primary lexbuf =
    let t = TokenStack.get lexbuf in
    match t with
    | NULL -> ENull
    | TRUE -> ETrue
    | FALSE -> EFalse
    | INT x -> EInt x
    | FLOAT x -> EFloat x
    | IDENT s -> EIdent s
    | LPAREN ->
        let e = parse_expr lexbuf in
        TokenStack.assert_and_consume lexbuf RPAREN;
        e
    | _ -> lexer_err @@ sprintf "expected a primary, got `%s'" (Token.to_str t)

(**
 * Helpers
 *)

and parse_identifiers ?(delim=COMMA) lexbuf =
    let ids = ref [assert_and_get_identifier lexbuf] in
    while TokenStack.match_and_consume lexbuf delim do
        ids := !ids @ [assert_and_get_identifier lexbuf]
    done;
    !ids

and parse_arguments lexbuf =
    let args = ref [parse_expr lexbuf] in
    while TokenStack.match_and_consume lexbuf COMMA do
        args := !args @ [parse_expr lexbuf]
    done;
    !args

and parse_block lexbuf =
    TokenStack.assert_and_consume lexbuf LBRACE;
    let block = parse_declaration lexbuf in
    TokenStack.assert_and_consume lexbuf RBRACE;
    block

let parse lexbuf =
    TokenStack.stack := [];
    parse_program lexbuf
