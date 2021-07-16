open Src
open Src.Value
open Printf

let compile lexbuf =
    let e = Parser.parse lexbuf in
    printf "Ast: %s\n" (Ast.stmt_to_str e);
    let env, v = Eval.eval e in
    printf "Env: %s\n" (Eval.env_to_str env);
    printf "Res: %s\n" (Value.to_str v)

let from_repl () =
    printf "Starting REPL...\n";
    while true do (
        printf "> ";
        let input = read_line () in
        let lexbuf = Lexing.from_string input in
        compile lexbuf
    ) done

let from_file fname =
    let file = open_in fname in
    let lexbuf = Lexing.from_channel file in
    compile lexbuf;
    close_in file

let () =
    let num_args = Array.length Sys.argv - 1 in
    if num_args = 0 then from_repl ()
    else from_file Sys.argv.(1)
