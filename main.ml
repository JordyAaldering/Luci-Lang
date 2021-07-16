open Src
open Src.Ast
open Src.Env
open Src.Value
open Printf

let print_ast = false
let print_env = true

let compile ?(env=SMap.empty) lexbuf =
    let prog = Parser.parse lexbuf in
    let env = Eval.eval ~env prog in
    if print_ast then printf "%s\n" (Ast.to_str prog);
    if print_env then printf "Env: %s\n" (Env.to_str env Value.to_str);
    env

let from_repl () =
    printf "Starting REPL...\n";
    let env = ref SMap.empty in
    while true do (
        printf "> ";
        let input = read_line () in
        let lexbuf = Lexing.from_string input in
        env := compile ~env:!env lexbuf
    ) done

let from_file fname =
    let file = open_in fname in
    let lexbuf = Lexing.from_channel file in
    let _ = compile lexbuf in
    close_in file

let () =
    let num_args = Array.length Sys.argv - 1 in
    if num_args = 0 then from_repl ()
    else from_file Sys.argv.(1)
