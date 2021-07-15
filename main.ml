open Src
open Printf

let repl () =
    printf "Starting REPL...\n";
    while true do (
        printf "> ";
        let input = read_line () in
        let lexbuf = Lexing.from_string input in
        let e = Parser.parse lexbuf in
        let _env = Eval.eval e in
        printf "\nAst:\n%s\n\n" (Ast.stmt_to_str e)
    ) done

let compile fname =
    let file = open_in fname in
    let lexbuf = Lexing.from_channel file in
    let e = Parser.parse lexbuf in
    let _env = Eval.eval e in
    printf "\nAst:\n%s\n\n" (Ast.stmt_to_str e);
    close_in file

let () =
    let num_args = Array.length Sys.argv - 1 in
    if num_args = 0 then repl ()
    else compile Sys.argv.(1)
