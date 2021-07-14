open Value
open Printf

module StringMap = Map.Make(String)

type envs = string StringMap.t * value StringMap.t

exception Env_error of string

let env_err msg =
    raise @@ Env_error msg

module Env = struct

let mk_empty () =
    (StringMap.empty, StringMap.empty)

let fst env =
    match env with
    | ptr_env, _ -> ptr_env

let snd env =
    match env with
    | _, val_env -> val_env

let ptr_env_to_str ptr_env =
    if StringMap.is_empty ptr_env then "[]"
    else
        StringMap.fold (fun var ptr tail ->
            sprintf "%s -> %s%s" var ptr
                (if tail = "" then "" else ", " ^ tail)
        ) ptr_env ""

let val_env_to_str val_env =
    if StringMap.is_empty val_env then "[]"
    else
        StringMap.fold (fun ptr value tail ->
            sprintf "%s -> %s%s" ptr (Value.to_str value)
                (if tail = "" then "" else ", " ^ tail)
        ) val_env ""

let to_str env =
    sprintf "ptr_env: [%s]\nval_env: [%s]"
        (ptr_env_to_str (fst env)) (val_env_to_str (snd env))

(**
 * Pointer helpers
 *)

let ptr_count = ref 0

let create_fresh_ptr () =
    ptr_count := !ptr_count + 1;
    sprintf "ptr%d" !ptr_count

let update_value_ptr env ptr_old ptr_new =
    let value_updater v = 
        match v with
        | VClosure (args, e, ptr_env) ->
            let new_env = StringMap.map (fun ptr ->
                if ptr = ptr_old then ptr_new else ptr
            ) ptr_env in
            VClosure (args, e, new_env)
        | _ -> v
    in
    let val_env = StringMap.map value_updater (snd env) in
    (fst env, val_env)

(**
 * Setters
 *)

let add_ptr env var ptr =
    let ptr_env = StringMap.add var ptr (fst env) in
    (ptr_env, snd env)

let add_val env ptr value =
    let val_env = StringMap.add ptr value (snd env) in
    (fst env, val_env)

let add_fresh_name env var =
    let ptr = create_fresh_ptr () in
    let ptr_env = StringMap.add var ptr (fst env) in
    (ptr, (ptr_env, snd env))

let add_fresh_value env value =
    let ptr = create_fresh_ptr () in
    let val_env = StringMap.add ptr value (snd env) in
    (ptr, (fst env, val_env))

let update_ptr env var new_ptr =
    if not (StringMap.mem var (fst env)) then
        env_err @@ sprintf "key `%s' is not in pointer env" var;
    let ptr_env = StringMap.add var new_ptr (fst env) in
    (ptr_env, snd env)

let update_val env ptr new_value =
    if not (StringMap.mem ptr (snd env)) then
        env_err @@ sprintf "key `%s' is not in value env" ptr;
    let val_env = StringMap.add ptr new_value (snd env) in
    (fst env, val_env)

(**
 * Getters
 *)

let find_ptr_from_name env var =
    try
        StringMap.find var (fst env)
    with Not_found ->
        env_err @@ sprintf "could not find variable `%s' in pointer env [%s]"
            var (ptr_env_to_str (fst env))

let find_value_from_ptr env ptr =
    try
        StringMap.find ptr (snd env)
    with Not_found ->
        env_err @@ sprintf "could not find pointer `%s' in value env [%s]"
            ptr (val_env_to_str (snd env))

let find_value_from_name env var =
    let ptr = find_ptr_from_name env var in
    find_value_from_ptr env ptr

end (* Env *)
