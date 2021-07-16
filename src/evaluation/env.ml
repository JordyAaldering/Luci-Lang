open Printf

exception Env_err of string

let env_err msg =
    raise @@ Env_err msg

module SMap = Map.Make(String)

(**
 * A wrapper class around the string map
 *)
module Env = struct

let to_str env val_to_str =
    if SMap.is_empty env then "[]"
    else
        sprintf "[%s]" (
            SMap.fold (fun k v tl ->
                sprintf "%s -> %s%s" k (val_to_str v)
                    (if tl = "" then "" else ", " ^ tl)
            ) env ""
        )

let keys_to_str env =
    if SMap.is_empty env then "[]"
    else
        sprintf "[%s]" (
            SMap.fold (fun k _v tl ->
                sprintf "%s%s" k (if tl = "" then "" else ", " ^ tl)
            ) env ""
        )

let find env k =
    try
        SMap.find k env
    with Not_found ->
        env_err @@ sprintf "could not find key `%s' in `%s'" k (keys_to_str env)

let add env k v =
    SMap.add k v env

end (* Env *)
