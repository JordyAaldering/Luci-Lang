open Ast
open Env
open Value

exception Eval_error of string

let eval_err msg =
    raise @@ Eval_error msg

let value_eval_binary op =
    match op with
    (* comparisons *)
    | OpEq -> Value.eq
    | OpNe -> Value.ne
    | OpLt -> Value.lt
    | OpLe -> Value.le
    | OpGt -> Value.gt
    | OpGe -> Value.ge
    | OpAnd -> Value.and_lazy
    | OpOr -> Value.or_lazy
    (* operations *)
    | OpPlus -> Value.add
    | OpMin -> Value.subtract
    | OpMult -> Value.multiply
    | OpDiv -> Value.divide
    | OpMod -> Value.modulo

let value_eval_unary op =
    match op with
    | OpNeg -> Value.negate
    | OpNot -> Value.not

let rec eval_expr env e =
    match e with
    (* values *)
    | ENull -> Env.add_fresh_value env VNull
    | ETrue -> Env.add_fresh_value env VTrue
    | EFalse -> Env.add_fresh_value env VFalse
    (* variables *)
    | EIdent s -> (Env.find_ptr_from_name env s, env)
    | EInt x -> Env.add_fresh_value env (VInt x)
    | EFloat x -> Env.add_fresh_value env (VFloat x)
    (* expressions *)
    | ECall (e, arg_exprs) ->
        let ptr1, env = eval_expr env e in
        let func = Env.find_value_from_ptr env ptr1 in
        let arg_names, e, func_ptr_env = Value.extract_closure func in

        let rec add_args_to_env arg_names arg_exprs env =
            match arg_names, arg_exprs with
            | (name :: t1), (expr :: t2) ->
                let ptr, env = eval_expr env expr in
                let env = Env.add_ptr env name ptr in
                add_args_to_env t1 t2 env
            | _, _ -> env
        in

        let _func_env = (func_ptr_env, Env.snd env) in
        let func_env = add_args_to_env arg_names arg_exprs env in
        eval_expr func_env e
    | ESelect (_var, _ids) -> ("", env)
    | EFunc (fname, args, e) ->
        let func_ptr, env = Env.add_fresh_name env fname in
        let env = Env.add_val env func_ptr (VClosure (args, e, Env.fst env)) in
        (func_ptr, env)
    | EVar (vname, e) ->
        let res_ptr, env = eval_expr env e in
        let env = Env.add_ptr env vname res_ptr in
        (res_ptr, env)
    | EAssign (vname, e) ->
        let ptr_new, env = eval_expr env e in
        let env = Env.update_ptr env vname ptr_new in
        (ptr_new, env)
    | ECond (e1, e2, e3) ->
        let ptr1, env = eval_expr env e1 in
        let v1 = Value.convert_to_bool @@ Env.find_value_from_ptr env ptr1 in
        if v1 then eval_expr env e2 else eval_expr env e3
    | EBinary (op, e1, e2) ->
        let ptr1, env = eval_expr env e1 in
        let ptr2, env = eval_expr env e2 in
        let v1 = Env.find_value_from_ptr env ptr1 in
        let v2 = Env.find_value_from_ptr env ptr2 in
        let res = value_eval_binary op v1 v2 in
        Env.add_fresh_value env res
    | EUnary (op, e) ->
        let ptr, env = eval_expr env e in
        let v = Env.find_value_from_ptr env ptr in
        let res = value_eval_unary op v in
        Env.add_fresh_value env res

let eval declarations =
    let last_ptr, env = List.fold_left (fun (_ptr, env) declaration ->
            eval_expr env declaration
        ) ("", Env.mk_empty ()) declarations
    in
    let last_val = Env.find_value_from_ptr env last_ptr in
    (last_val, env)
