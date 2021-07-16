open Src
open Src.Env
open Src.Value
open Printf

let compile lexbuf =
    let ast = Parser.parse lexbuf in
    let env = Eval.eval ast in
    printf "Ast: %s\n" (Ast.stmt_to_str ast);
    printf "Env: %s\n" (Env.to_str env Value.to_str)

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
