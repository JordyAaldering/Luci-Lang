open Ast
open Value
open Printf

exception Eval_error of string

let eval_err msg =
    raise @@ Eval_error msg

module Env = Map.Make(String)

let value_eval_binary op =
    match op with
    | OpOr -> Value.or_lazy
    | OpAnd -> Value.and_lazy
    | OpEq -> Value.eq
    | OpNe -> Value.ne
    | OpLt -> Value.lt
    | OpLe -> Value.le
    | OpGt -> Value.gt
    | OpGe -> Value.ge
    | OpPlus -> Value.add
    | OpMin -> Value.subtract
    | OpMult -> Value.multiply
    | OpDiv -> Value.divide
    | OpMod -> Value.modulo

let value_eval_unary op =
    match op with
    | OpNeg -> Value.negate
    | OpNot -> Value.not

let rec eval_decl env e =
    match e with
    | DeclFun (id, args, e) ->
        let cls = VClosure (args, e, env) in
        let env = Env.add id cls env in
        env
    | DeclVar (id, e) ->
        let v = eval_expr env e in
        let env = Env.add id v env in
        env
    | DeclStmt s ->
        eval_stmt env s

and eval_stmt env e =
    match e with
    | StmtAssign (id, e) ->
        if not (Env.mem id env) then
            eval_err @@ sprintf "key `%s' is not in env" id;
        let v = eval_expr env e in
        let env = Env.add id v env in
        env
    | StmtCond (e, st, sf) ->
        let v = eval_expr env e in
        let cond = Value.convert_to_bool v in
        eval_stmt env (if cond then st else sf)
    | StmtPrint e ->
        let v = eval_expr env e in
        printf "%s\n" (Value.to_str v);
        env
    | StmtBlock ds ->
        List.fold_left (fun env' d ->
                eval_decl env' d
            ) env ds
    | StmtExpr e ->
        let _ = eval_expr env e in
        env

and eval_expr env e =
    match e with
    | ExprBinary (op, e1, e2) ->
        let v1 = eval_expr env e1 in
        let v2 = eval_expr env e2 in
        value_eval_binary op v1 v2
    | ExprUnary (op, e) ->
        let v = eval_expr env e in
        value_eval_unary op v
    | ExprCall (_e, _es) ->
        VNull
    | ExprSelect (_e, _ids) ->
        VNull
    (* primary *)
    | ExprNull -> VNull
    | ExprTrue -> VTrue
    | ExprFalse -> VFalse
    | ExprInt x -> VInt x
    | ExprFloat x -> VFloat x
    | ExprIdent id -> Env.find id env

(*
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
    | EBlock exprs ->
        List.fold_left (fun (_ptr, env) e -> eval_expr env e) ("", env) exprs
    | ECall (e, arg_exprs) ->
        let fptr, env = eval_expr env e in
        let func = Env.find_value_from_ptr env fptr in
        let arg_names, e, func_ptr_env = Value.extract_closure func in

        let rec add_args_to_env arg_names arg_exprs fenv =
            match arg_names, arg_exprs with
            | (name :: t1), (expr :: t2) ->
                let ptr, fenv = eval_expr env expr in
                let fenv = Env.add_ptr fenv name ptr in
                add_args_to_env t1 t2 fenv
            | _, _ -> fenv
        in

        let fenv = (func_ptr_env, Env.snd env) in
        let fenv = add_args_to_env arg_names arg_exprs fenv in
        eval_expr fenv e
    | ESelect (_var, _ids) -> ("", env)
    | EFunc (fname, args, e) ->
        let fptr, env = Env.add_fresh_ptr env fname in
        let func = VClosure (args, e, Env.fst env) in
        let env = Env.add_value env fptr func in
        (fptr, env)
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
    | EPrint e ->
        let ptr, env = eval_expr env e in
        let v = Env.find_value_from_ptr env ptr in
        printf "%s\n" (Value.to_str v);
        (ptr, env)
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
*)

let eval e =
    eval_stmt Env.empty e
