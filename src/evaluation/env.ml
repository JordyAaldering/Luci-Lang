open Value
open Printf

module StringMap = Map.Make(String)

type envs = string StringMap.t * value StringMap.t

exception Env_error of string

let env_err msg =
    raise @@ Env_error msg

(**
 * Module for keeping track of and operating on the program environment.
 * An environment is a tuple of (ptr_env, val_env),
 * where ptr_env is a mapping from variable names to pointers
 * and val_env is a mapping from pointers to values.
 *)
module Env = struct

let mk_empty () =
    StringMap.empty, StringMap.empty

let fst env =
    match env with
    | ptr_env, _ -> ptr_env

let snd env =
    match env with
    | _, val_env -> val_env

let ptr_env_to_str ptr_env =
    if StringMap.is_empty ptr_env then "[]"
    else
        StringMap.fold (fun vname ptr tl ->
            sprintf "%s -> %s%s" vname ptr
                (if tl = "" then "" else ", " ^ tl)
        ) ptr_env ""

let val_env_to_str val_env =
    if StringMap.is_empty val_env then "[]"
    else
        StringMap.fold (fun ptr value tl ->
            sprintf "%s -> %s%s" ptr (Value.to_str value)
                (if tl = "" then "" else ", " ^ tl)
        ) val_env ""

let to_str env =
    sprintf "Pointer env: [%s]\nValue env: [%s]"
        (ptr_env_to_str (fst env)) (val_env_to_str (snd env))

(**
 * Pointer helpers
 *)

let ptr_count = ref 0

let create_fresh_ptr () =
    ptr_count := !ptr_count + 1;
    sprintf "p%d" !ptr_count

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

let add_ptr env vname ptr =
    let ptr_env = StringMap.add vname ptr (fst env) in
    (ptr_env, snd env)

let add_value env ptr value =
    let val_env = StringMap.add ptr value (snd env) in
    (fst env, val_env)

let add_fresh_ptr env vname =
    let ptr = create_fresh_ptr () in
    (ptr, add_ptr env vname ptr)

let add_fresh_value env value =
    let ptr = create_fresh_ptr () in
    (ptr, add_value env ptr value)

let update_ptr env vname ptr =
    if not (StringMap.mem vname (fst env)) then
        env_err @@ sprintf "key `%s' is not in pointer env" vname;
    add_ptr env vname ptr

let update_value env ptr value =
    if not (StringMap.mem ptr (snd env)) then
        env_err @@ sprintf "key `%s' is not in value env" ptr;
    add_value env ptr value

(**
 * Getters
 *)

let find_ptr_from_name env vname =
    try
        StringMap.find vname (fst env)
    with Not_found ->
        env_err @@ sprintf "could not find variable `%s' in pointer env [%s]"
            vname (ptr_env_to_str (fst env))

let find_value_from_ptr env ptr =
    try
        StringMap.find ptr (snd env)
    with Not_found ->
        env_err @@ sprintf "could not find pointer `%s' in value env [%s]"
            ptr (val_env_to_str (snd env))

end (* Env *)
