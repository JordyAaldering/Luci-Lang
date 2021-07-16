open Ast
open Token
open Lexer
open Printf

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

    let assert_and_get_ident lexbuf =
        let t = get lexbuf in
        match t with
        | IDENT id -> id
        | _ -> parse_err @@ sprintf "expected an identifier, got `%s'" (Token.to_str t)
end (* TokenStack *)

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
    | CLASS -> parse_class_decl lexbuf
    | FUNCTION -> parse_function_decl lexbuf
    | VAR -> parse_var_decl lexbuf
    (* fallthrough case *)
    | _ -> DeclStmt (parse_statement lexbuf)

and parse_class_decl lexbuf =
    TokenStack.assert_and_consume lexbuf CLASS;
    let id = TokenStack.assert_and_get_ident lexbuf in
    
    TokenStack.assert_and_consume lexbuf LBRACE;
    let block = ref [] in
    while not (TokenStack.match_and_consume lexbuf RBRACE) do
        let t = TokenStack.peek lexbuf in
        let decl = match t with
            | FUNCTION -> parse_function_decl lexbuf
            | VAR -> parse_var_decl lexbuf
            | _ -> parse_err @@ sprintf "classes can only contain functions and variables, got `%s'" (Token.to_str t)
        in
        block := !block @ [decl]
    done;
    DeclClass (id, !block)

and parse_function_decl lexbuf =
    TokenStack.assert_and_consume lexbuf FUNCTION;
    let id = TokenStack.assert_and_get_ident lexbuf in

    TokenStack.assert_and_consume lexbuf LPAREN;
    let args = if not (TokenStack.match_and_consume lexbuf RPAREN) then
            let args = parse_identifiers lexbuf in
            TokenStack.assert_and_consume lexbuf RPAREN;
            args
        else []
    in

    let block = parse_block_stmt lexbuf in
    DeclFun (id, args, block)

and parse_var_decl lexbuf =
    TokenStack.assert_and_consume lexbuf VAR;
    let id = TokenStack.assert_and_get_ident lexbuf in

    let e = if TokenStack.match_and_consume lexbuf EQ then
            parse_expr lexbuf
        else ExprNull
    in
    TokenStack.assert_and_consume lexbuf SEMICOLON;
    DeclVar (id, e)

(**
 * Statements
 *)

and parse_statement lexbuf =
    let t = TokenStack.peek lexbuf in
    match t with
    | IF -> parse_cond_stmt lexbuf
    | WHILE -> parse_while_stmt lexbuf
    | FOR -> parse_for_stmt lexbuf
    | RETURN -> parse_return_stmt lexbuf
    | PRINT -> parse_print_stmt lexbuf
    | LBRACE -> parse_block_stmt lexbuf
    (* fallthrough case *)
    | _ -> parse_expr_stmt lexbuf

and parse_cond_stmt lexbuf =
    TokenStack.assert_and_consume lexbuf IF;
    let cond = parse_expr lexbuf in
    TokenStack.assert_and_consume lexbuf THEN;
    let st = parse_statement lexbuf in
    
    let sf = if TokenStack.match_and_consume lexbuf ELSE then
            parse_statement lexbuf
        else StmtExpr ExprNull
    in
    StmtCond (cond, st, sf)

and parse_while_stmt lexbuf =
    TokenStack.assert_and_consume lexbuf WHILE;
    let cond = parse_expr lexbuf in
    TokenStack.assert_and_consume lexbuf DO;
    let st = if not (TokenStack.match_and_consume lexbuf SEMICOLON) then
            parse_statement lexbuf
        else StmtExpr ExprNull
    in
    StmtWhile (cond, st)

and parse_for_stmt lexbuf = 
    TokenStack.assert_and_consume lexbuf FOR;
    let init = if not (TokenStack.match_and_consume lexbuf SEMICOLON) then
            parse_declaration lexbuf
        else DeclStmt (StmtExpr ExprNull)
    in
    let cond = if not (TokenStack.match_and_consume lexbuf SEMICOLON) then
            let cond = parse_expr lexbuf in
            TokenStack.assert_and_consume lexbuf SEMICOLON;
            cond
        else ExprNull
    in
    let step = if not (TokenStack.match_and_consume lexbuf DO) then
            let step = parse_statement lexbuf in
            TokenStack.assert_and_consume lexbuf DO;
            step
        else StmtExpr ExprNull
    in
    let s = if not (TokenStack.match_and_consume lexbuf SEMICOLON) then
            parse_statement lexbuf
        else StmtExpr ExprNull
    in
    StmtFor (init, cond, step, s)

and parse_return_stmt lexbuf =
    TokenStack.assert_and_consume lexbuf RETURN;
    let e = if not (TokenStack.match_and_consume lexbuf SEMICOLON) then
            let e = parse_expr lexbuf in
            TokenStack.assert_and_consume lexbuf SEMICOLON;
            e
        else ExprNull
    in
    StmtReturn e

and parse_print_stmt lexbuf =
    TokenStack.assert_and_consume lexbuf PRINT;
    TokenStack.assert_and_consume lexbuf LPAREN;
    let e = parse_expr lexbuf in
    TokenStack.assert_and_consume lexbuf RPAREN;
    TokenStack.assert_and_consume lexbuf SEMICOLON;
    StmtPrint e

and parse_block_stmt lexbuf =
    TokenStack.assert_and_consume lexbuf LBRACE;
    let block = ref [] in
    while not (TokenStack.match_and_consume lexbuf RBRACE) do
        block := !block @ [parse_declaration lexbuf]
    done;
    StmtBlock !block

and parse_expr_stmt lexbuf =
    let e = parse_expr lexbuf in
    TokenStack.assert_and_consume lexbuf SEMICOLON;
    StmtExpr e

(**
 * Expressions
 *)

and parse_expr lexbuf =
    parse_assign_stmt lexbuf

and parse_assign_stmt lexbuf =
    let e1 = parse_binary lexbuf in
    if TokenStack.match_and_consume lexbuf EQ then
        let e2 = parse_assign_stmt lexbuf in
        ExprAssign (e1, e2)
    else e1

and parse_binary lexbuf =
    let rec resolve_stack s prec =
        let e1, op1, prec1 = Stack.pop s in
        if prec <= prec1 then (
            let e2, op2, prec2 = Stack.pop s in
            let e = ExprBinary (Token.to_bop op1, e2, e1) in
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
        ExprUnary (op, e)
    else parse_call lexbuf

and parse_call lexbuf =
    let e = parse_primary lexbuf in
    let t = TokenStack.peek lexbuf in
    match t with
    | LPAREN -> begin
        TokenStack.assert_and_consume lexbuf LPAREN;
        let args = parse_arguments lexbuf in
        TokenStack.assert_and_consume lexbuf RPAREN;
        ExprCall (e, args)
    end
    | DOT -> begin
        TokenStack.assert_and_consume lexbuf DOT;
        let ids = parse_identifiers ~delim:DOT lexbuf in
        ExprSelect (e, ids)
    end
    | _ -> e

and parse_primary lexbuf =
    let t = TokenStack.get lexbuf in
    match t with
    | NULL  -> ExprNull
    | TRUE  -> ExprTrue
    | FALSE -> ExprFalse
    | INT x -> ExprInt x
    | FLOAT x -> ExprFloat x
    | IDENT id -> ExprIdent id
    | LPAREN ->
        let e = parse_expr lexbuf in
        TokenStack.assert_and_consume lexbuf RPAREN;
        e
    | _ -> parse_err @@ sprintf "expected a primary, got `%s'" (Token.to_str t)

(**
 * Helpers
 *)

and parse_identifiers ?(delim=COMMA) lexbuf =
    let ids = ref [TokenStack.assert_and_get_ident lexbuf] in
    while TokenStack.match_and_consume lexbuf delim do
        ids := !ids @ [TokenStack.assert_and_get_ident lexbuf]
    done;
    !ids

and parse_arguments lexbuf =
    let args = ref [parse_expr lexbuf] in
    while TokenStack.match_and_consume lexbuf COMMA do
        args := !args @ [parse_expr lexbuf]
    done;
    !args

let parse lexbuf =
    TokenStack.stack := [];
    let prog = parse_program lexbuf in
    printf "Parsed %d global declarations\n" (List.length prog);
    prog
