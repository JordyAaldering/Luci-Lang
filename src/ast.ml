open Printf

(**
 * A declaration binds identifiers to statements.
 *)
type decl =
    | DeclFun of string * string list * decl
    | DeclVar of string * expr
    | DeclStmt of stmt

(**
 * A statement does something with potential side-effects,
 * evaluating to an updated environment.
 *)
and stmt =
    | StmtAssign of string * expr
    | StmtCond of expr * stmt * stmt
    | StmtPrint of expr
    | StmtExpr of expr

(**
 * An expression evaluates to a value.
 *)
and expr =
    | ExprCall of string * expr list
    | ExprSelect of expr * string list
    | ExprBinary of bop * expr * expr
    | ExprUnary of uop * expr
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
    | DeclFun (id, args, d) ->
        sprintf "function %s(%s) {%s}" id (String.concat ", " args) (decl_to_str d)
    | DeclVar (id, e) ->
        sprintf "var %s = %s" id (expr_to_str e)
    | DeclStmt s ->
        stmt_to_str s

and stmt_to_str stmt =
    | StmtAssign (id, e) ->
        sprintf "%s = %s" id (expr_to_str e)
    | StmtCond (e, s1, ExprNull) ->
        sprintf "if %s then %s" (expr_to_str e) (stmt_to_str s1)
    | StmtCond (e, s1, s2) ->
        sprintf "if %s then %s else %s" (expr_to_str e) (stmt_to_str s1) (stmt_to_str s2)
    | StmtPrint e ->
        sprintf "print(%s);" (expr_to_str e)
    | StmtExpr e ->
        sprintf "%s;" (expr_to_str e)

and expr_to_str expr =
    | ExprCall (id, args) ->
        sprintf "%s(%s)" id (String.concat ", " (List.map expr_to_str args))
    | ExprSelect (e, ids) ->
        sprintf "%s.%s" (expr_to_str e) (String.concat "." ids)
    | ExprBinary (op, e1, e2) ->
        sprintf "(%s %s %s)" (expr_to_str e1) (bop_to_str op) (expr_to_str e2)
    | ExprUnary (op, e) ->
        sprintf "%s%s" (uop_to_str op) (expr_to_str e)
    (* primary *)
    | ExprNull -> "null"
    | ExprTrue -> "true"
    | ExprFalse -> "false"
    | ExprInt x -> string_of_int 
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
