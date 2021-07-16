open Ast
open Env
open Printf

exception ValueError of string

let value_err msg =
    raise @@ ValueError msg

type value =
    | VNull
    | VTrue
    | VFalse
    | VInt of int
    | VFloat of float
    | VClosure of string list * stmt * value SMap.t

module Value = struct

let rec to_str v =
    match v with
    | VNull -> "null"
    | VTrue -> "true"
    | VFalse -> "false"
    | VInt x -> string_of_int x
    | VFloat x -> string_of_float x
    | VClosure (args, body, env) ->
        sprintf "{\\(%s).%s, %s}" (String.concat "," args)
            (Ast.stmt_to_str 0 true body) (Env.to_str env to_str)

(**
* Helper methods
*)

let is_numerical v =
    match v with
    | VInt _
    | VFloat _ -> true
    | _ -> false

let is_truthy v =
    match v with
    | VNull -> false
    | VTrue -> true
    | VFalse -> false
    | VInt x -> x <> 0
    | VFloat x -> x <> 0.
    | VClosure _ -> value_err "closure has no truth value"

(**
* Binary helper methods
*)

let convert_to_bool v =
    match v with
    | VNull -> false
    | VTrue -> true
    | VFalse -> false
    | VInt x -> x <> 0
    | VFloat x -> x <> 0.
    | VClosure _ -> value_err "Cannot convert closure to bool"

let convert_to_int v =
    match v with
    | VNull -> value_err "Cannot convert null to int"
    | VTrue
    | VFalse -> value_err "Cannot implicitly convert bool to int"
    | VInt x -> x
    | VFloat _ -> value_err "Cannot implicitly convert float to int"
    | VClosure _ -> value_err "Cannot convert closure to int"

let convert_to_float v =
    match v with
    | VNull -> value_err "Cannot convert null to float"
    | VTrue
    | VFalse -> value_err "Cannot implicitly convert bool to float"
    | VInt x -> float_of_int x
    | VFloat x -> x
    | VClosure _ -> value_err "Cannot convert closure to float"

let binary_int_op op v1 v2 =
    let f1 = convert_to_int v1 in
    let f2 = convert_to_int v2 in
    VInt (op f1 f2)

let binary_float_op op v1 v2 =
    let f1 = convert_to_float v1 in
    let f2 = convert_to_float v2 in
    VFloat (op f1 f2)

let binary_int_or_float_op int_op float_op v1 v2 =
    try binary_int_op int_op v1 v2
    with ValueError _ ->
        binary_float_op float_op v1 v2

let binary_bool_op op v1 v2 =
    let f1 = convert_to_bool v1 in
    let f2 = convert_to_bool v2 in
    if op f1 f2 then VTrue else VFalse

(**
* Comparison operations
*)

let binary_compare op v1 v2 =
    let f1 = convert_to_float v1 in
    let f2 = convert_to_float v2 in
    if op f1 f2 then VTrue else VFalse

let eq = binary_compare (=)
let ne = binary_compare (<>)
let lt = binary_compare (<)
let le = binary_compare (<=)
let gt = binary_compare (>)
let ge = binary_compare (>=)

let and_lazy = binary_bool_op (&&)
let or_lazy = binary_bool_op (||)

let not v =
    if convert_to_bool v then VFalse else VTrue

(**
* Math operations
*)

let add = binary_int_or_float_op (+) (+.)
let subtract = binary_int_or_float_op (-) (-.)
let multiply = binary_int_or_float_op ( * ) ( *.)
let divide = binary_int_or_float_op (/) (/.)
let modulo = binary_int_op (mod)

let negate v =
    match v with
    | VNull -> value_err "Cannot take negation of null"
    | VTrue
    | VFalse -> value_err "Cannot take negation of a bool"
    | VInt x -> VInt (-x)
    | VFloat x -> VFloat (-.x)
    | VClosure _ -> value_err "Cannot take negation of a closure"

let absolute v =
    match v with
    | VNull -> value_err "Cannot take absolute value of null"
    | VTrue
    | VFalse -> value_err "Cannot take absolute value of a bool"
    | VInt x -> VInt (abs x)
    | VFloat x -> VFloat (abs_float x)
    | VClosure _ -> value_err "Cannot take absolute value of a closure"

(**
* Helpers
*)

let extract_closure v =
    match v with
    | VClosure (args, e, env) -> (args, e, env)
    | _ -> value_err @@ sprintf "Invalid argument `%s'" (to_str v)

end (* Value *)
