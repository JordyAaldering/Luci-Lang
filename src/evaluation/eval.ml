open Ast
open Value
open Printf

exception Eval_error of string

let eval_err msg =
    raise @@ Eval_error msg

module Env = Map.Make(String)

let env_to_str env =
    if Env.is_empty env then "[]"
    else
        sprintf "[%s]" (
            Env.fold (fun id v tl ->
                sprintf "%s -> %s%s" id (Value.to_str v)
                    (if tl = "" then "" else ", " ^ tl)
            ) env ""
        )

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
    printf "decl: %s\n\t%s\n" (decl_to_str e) (env_to_str env);
    match e with
    | DeclFun (id, args, e) ->
        let cls = VClosure (args, e, env) in

        let args, e, envc = Value.extract_closure cls in
        let envc = Env.add id cls envc in
        let cls = VClosure (args, e, envc) in
        printf "--- %s\n" (env_to_str envc);
        
        let env = Env.add id cls env in
        env
    | DeclVar (id, e) ->
        let v = eval_expr env e in
        let env = Env.add id v env in
        env
    | DeclStmt s ->
        let env, _v = eval_stmt env s in
        env

and eval_stmt env e =
    printf "stmt: %s\n\t%s\n" (stmt_to_str e) (env_to_str env);
    match e with
    | StmtAssign (id, e) ->
        if not (Env.mem id env) then
            eval_err @@ sprintf "unknown identifier '%s' in `%s'" id (env_to_str env);
        let v = eval_expr env e in
        let env = Env.add id v env in
        env, v
    | StmtCond (e, st, sf) ->
        let v = eval_expr env e in
        let cond = Value.convert_to_bool v in
        eval_stmt env (if cond then st else sf)
    | StmtPrint e ->
        let v = eval_expr env e in
        printf "%s\n" (Value.to_str v);
        env, VNull
    | StmtBlock ds ->
        let env = List.fold_left (fun env' d ->
                eval_decl env' d
            ) env ds
        in
        env, VNull
    | StmtExpr e ->
        let v = eval_expr env e in
        env, v

and eval_expr env e =
    printf "expr: %s\n\t%s\n" (expr_to_str e) (env_to_str env);
    match e with
    | ExprReturn e ->
        let v = eval_expr env e in
        v
    | ExprBinary (op, e1, e2) ->
        let v1 = eval_expr env e1 in
        let v2 = eval_expr env e2 in
        value_eval_binary op v1 v2
    | ExprUnary (op, e) ->
        let v = eval_expr env e in
        value_eval_unary op v
    | ExprCall (ExprIdent id, es) ->
        let cls = try Env.find id env
            with Not_found ->
                eval_err @@ sprintf "unknown identifier '%s' in `%s'" id (env_to_str env);
        in
        let args, sf, envf = Value.extract_closure cls in
        let envf = List.fold_left2 (fun env' arg e ->
                let v = eval_expr env e in
                Env.add arg v env'
            ) envf args es
        in
        let _env, v = eval_stmt envf sf in
        v
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

let eval e =
    eval_stmt Env.empty e
