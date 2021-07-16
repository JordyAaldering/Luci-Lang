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
    | DeclClass (id, block) ->
        let class_env = List.fold_left (fun env' decl ->
                eval_decl env' decl
            ) env block
        in
        let v = VClass class_env in
        Env.add env id v
    | DeclFun (id, args, e) ->
        let cls = VClosure (args, e, env) in
        Env.add env id cls
    | DeclVar (id, e) ->
        let v, env = eval_expr env e in
        Env.add env id v
    | DeclStmt s ->
        eval_stmt env s

and eval_stmt env e =
    match e with
    | StmtCond (cond, st, sf) ->
        let v, env = eval_expr env cond in
        let cond = Value.convert_to_bool v in
        eval_stmt env (if cond then st else sf)
    | StmtWhile (cond, s) ->
        let v, env = eval_expr env cond in
        let v_ref = ref v in
        let env_ref = ref env in
        while Value.is_truthy @@ !v_ref do
            env_ref := eval_stmt !env_ref s;
            let v, env = eval_expr !env_ref cond in
            v_ref := v;
            env_ref := env
        done;
        !env_ref
    | StmtFor (init, cond, step, s) ->
        let env = eval_decl env init in
        let v, env = eval_expr env cond in
        let v_ref = ref v in
        let env_ref = ref env in
        while Value.is_truthy @@ !v_ref do
            env_ref := eval_stmt !env_ref s;
            env_ref := eval_stmt !env_ref step;
            let v, env = eval_expr !env_ref cond in
            v_ref := v;
            env_ref := env
        done;
        !env_ref
    | StmtReturn e ->
        let v, _env = eval_expr env e in
        raise @@ Return v
    | StmtPrint e ->
        let v, env = eval_expr env e in
        printf "%s\n" (Value.to_str v);
        env
    | StmtBlock ds ->
        List.fold_left (fun env' d ->
            eval_decl env' d
        ) env ds
    | StmtExpr e ->
        let _v, env = eval_expr env e in
        env

and eval_expr env e =
    match e with
    | ExprAssign (ExprIdent id, e) ->
        let v, env = eval_expr env e in
        let env = Env.add env id v in
        v, env
    | ExprAssign (_e1, e2) ->
        eval_expr env e2
    | ExprBinary (op, e1, e2) ->
        let v1, env = eval_expr env e1 in
        let v2, env = eval_expr env e2 in
        value_eval_binary op v1 v2, env
    | ExprUnary (op, e) ->
        let v, env = eval_expr env e in
        value_eval_unary op v, env
    | ExprCall (e, es) ->
        let cls, env = eval_expr env e in
        let args, f_body, f_env = Value.extract_closure cls in
        let f_env = List.fold_left2 (fun env' arg e ->
                let v, _env = eval_expr env e in
                Env.add env' arg v
            ) f_env args es
        in
        (try let env = eval_stmt f_env f_body in VNull, env
            with Return r -> r, env)
    | ExprSelect (_e, _ids) ->
        eval_err "not yet implemented"
    (* primary *)
    | ExprNull -> VNull, env
    | ExprTrue -> VTrue, env
    | ExprFalse -> VFalse, env
    | ExprInt x -> VInt x, env
    | ExprFloat x -> VFloat x, env
    | ExprIdent id -> Env.find env id, env

let eval ?(env=SMap.empty) prog =
    List.fold_left (fun env' decl ->
        eval_decl env' decl
    ) env prog
