open Src
open Src.Value
open Src.Env
open Printf

let repl () =
    printf "Starting REPL...\n";
    while true do (
        printf "> ";
        let input = read_line () in
        let lexbuf = Lexing.from_string input in
        let declarations = Parser.parse lexbuf in
        let v, env = Eval.eval declarations in
        printf "\nDeclarations:\n%s\n\n" (String.concat "\n" (List.map Ast.expr_to_str declarations));
        printf "Enviroment:\n%s\n\n" (Env.to_str env);
        printf "Res: %s\n" (Value.to_str v)
    ) done

let compile fname =
    let file = open_in fname in
    let lexbuf = Lexing.from_channel file in
    let declarations = Parser.parse lexbuf in
    close_in file;
    let v, env = Eval.eval declarations in
    printf "\nDeclarations:\n%s\n\n" (String.concat "\n" (List.map Ast.expr_to_str declarations));
    printf "Enviroment:\n%s\n\n" (Env.to_str env);
    printf "Res: %s\n" (Value.to_str v)

let () =
    let num_args = Array.length Sys.argv - 1 in
    if num_args = 0 then repl ()
    else compile Sys.argv.(1)
