open Printf

(**
 * A declaration binds identifiers to statements.
 *)
type decl =
    | DeclFun of string * string list * stmt
    | DeclVar of string * expr
    | DeclStmt of stmt

(**
 * A statement does something with potential side-effects,
 * evaluating to an updated environment.
 *)
and stmt =
    | StmtCond of expr * stmt * stmt
    | StmtReturn of expr
    | StmtPrint of expr
    | StmtBlock of decl list
    | StmtExpr of expr

(**
 * An expression evaluates to a value.
 *)
and expr =
    | ExprAssign of expr * expr
    | ExprBinary of bop * expr * expr
    | ExprUnary of uop * expr
    | ExprCall of expr * expr list
    | ExprSelect of expr * string list
    (* primary *)
    | ExprNull
    | ExprTrue
    | ExprFalse
    | ExprInt of int
    | ExprFloat of float
    | ExprIdent of string

and bop =
    | OpOr
    | OpAnd
    | OpEq
    | OpNe
    | OpLt
    | OpLe
    | OpGt
    | OpGe
    | OpPlus
    | OpMin
    | OpMult
    | OpDiv
    | OpMod

and uop =
    | OpNeg
    | OpNot

let rec decl_to_str decl =
    match decl with
    | DeclFun (id, args, block) ->
        sprintf "function %s(%s) %s" id (String.concat ", " args) (stmt_to_str block)
    | DeclVar (id, e) ->
        sprintf "var %s = %s;" id (expr_to_str e)
    | DeclStmt s ->
        stmt_to_str s

and stmt_to_str stmt =
    match stmt with
    | StmtCond (e, s1, StmtExpr ExprNull) ->
        sprintf "if %s then %s" (expr_to_str e) (stmt_to_str s1)
    | StmtCond (e, s1, s2) ->
        sprintf "if %s then %s else %s" (expr_to_str e) (stmt_to_str s1) (stmt_to_str s2)
    | StmtReturn e ->
        sprintf "return %s;" (expr_to_str e)
    | StmtPrint e ->
        sprintf "print(%s);" (expr_to_str e)
    | StmtBlock block ->
        sprintf "{%s}" (String.concat " " (List.map decl_to_str block))
    | StmtExpr e ->
        sprintf "%s;" (expr_to_str e)

and expr_to_str expr =
    match expr with
    | ExprAssign (e1, e2) ->
        sprintf "%s = %s" (expr_to_str e1) (expr_to_str e2)
    | ExprBinary (op, e1, e2) ->
        sprintf "(%s %s %s)" (expr_to_str e1) (bop_to_str op) (expr_to_str e2)
    | ExprUnary (op, e) ->
        sprintf "%s%s" (uop_to_str op) (expr_to_str e)
    | ExprCall (e, args) ->
        sprintf "%s(%s)" (expr_to_str e) (String.concat ", " (List.map expr_to_str args))
    | ExprSelect (e, ids) ->
        sprintf "%s.%s" (expr_to_str e) (String.concat "." ids)
    (* primary *)
    | ExprNull -> "null"
    | ExprTrue -> "true"
    | ExprFalse -> "false"
    | ExprInt x -> string_of_int x
    | ExprFloat x -> string_of_float x
    | ExprIdent id -> id

and bop_to_str op =
    match op with
    | OpOr   -> "||"
    | OpAnd  -> "&&"
    | OpEq   -> "="
    | OpNe   -> "!="
    | OpLt   -> "<"
    | OpLe   -> "<="
    | OpGt   -> ">"
    | OpGe   -> ">="
    | OpPlus -> "+"
    | OpMin  -> "-"
    | OpMult -> "*"
    | OpDiv  -> "/"
    | OpMod  -> "%"

and uop_to_str op =
    match op with
    | OpNeg -> "-"
    | OpNot -> "!"
