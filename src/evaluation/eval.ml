open Ast
open Env
open Value
open Printf

exception Eval_error of string

exception Return of value

let eval_err msg =
    raise @@ Eval_error msg

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
        Env.add env id cls
    | DeclVar (id, e) ->
        let v = eval_expr env e in
        Env.add env id v
    | DeclStmt s ->
        eval_stmt env s

and eval_stmt env e =
    match e with
    | StmtCond (e, st, sf) ->
        let v = eval_expr env e in
        let cond = Value.convert_to_bool v in
        eval_stmt env (if cond then st else sf)
    | StmtReturn e ->
        let v = eval_expr env e in
        raise @@ Return v
    | StmtPrint e ->
        let v = eval_expr env e in
        printf "%s\n" (Value.to_str v);
        env
    | StmtBlock ds ->
        List.fold_left (fun env' d ->
            eval_decl env' d
        ) env ds
    | StmtExpr e ->
        let _v = eval_expr env e in
        env

and eval_expr env e =
    match e with
    | ExprAssign (_e1, e2) ->
        eval_expr env e2
    | ExprBinary (op, e1, e2) ->
        let v1 = eval_expr env e1 in
        let v2 = eval_expr env e2 in
        value_eval_binary op v1 v2
    | ExprUnary (op, e) ->
        let v = eval_expr env e in
        value_eval_unary op v
    | ExprCall (e, es) ->
        let cls = eval_expr env e in
        let args, f_body, f_env = Value.extract_closure cls in
        let f_env = List.fold_left2 (fun env' arg e ->
                let v = eval_expr env e in
                Env.add env' arg v
            ) f_env args es
        in
        (try let _env = eval_stmt f_env f_body in VNull
            with Return r -> r)
    | ExprSelect (_e, _ids) ->
        eval_err "not yet implemented"
    (* primary *)
    | ExprNull -> VNull
    | ExprTrue -> VTrue
    | ExprFalse -> VFalse
    | ExprInt x -> VInt x
    | ExprFloat x -> VFloat x
    | ExprIdent id -> Env.find env id

let eval ?(env=SMap.empty) prog =
    List.fold_left (fun env' decl ->
        eval_decl env' decl
    ) env prog
