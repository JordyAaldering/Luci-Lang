open Printf

(** A program is a series of declarations. *)
type program = decl list

(** A declaration binds identifiers to statements. *)
and decl =
    | DeclFun of string * string list * stmt
    | DeclVar of string * expr
    | DeclStmt of stmt

(** A statement does something with potential side-effects. *)
and stmt =
    | StmtCond of expr * stmt * stmt
    | StmtReturn of expr
    | StmtPrint of expr
    | StmtBlock of decl list
    | StmtExpr of expr

(** An expression evaluates to a value. *)
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

module Ast = struct

let rec whitespace depth =
    if depth <= 0 then ""
    else "    " ^ (whitespace (depth - 1))

let rec decl_to_str depth flatten decl =
    match decl with
    | DeclFun (id, args, block) ->
        if flatten then
            sprintf "function %s(%s) %s"
                id (String.concat ", " args) (stmt_to_str 0 false block)
        else
            sprintf "\nfunction %s(%s) %s\n"
                id (String.concat ", " args) (stmt_to_str (depth + 1) flatten block)
    | DeclVar (id, e) ->
        sprintf "var %s = %s;"
            id (expr_to_str depth flatten e)
    | DeclStmt s ->
        stmt_to_str depth flatten s

and stmt_to_str depth flatten stmt =
    match stmt with
    | StmtCond (e, s1, StmtExpr ExprNull) ->
        sprintf "if %s then %s"
            (expr_to_str depth flatten e) (stmt_to_str depth flatten s1)
    | StmtCond (e, s1, s2) ->
        sprintf "if %s then %s else %s"
            (expr_to_str depth flatten e) (stmt_to_str depth flatten s1) (stmt_to_str depth flatten s2)
    | StmtReturn e ->
        sprintf "return %s;"
            (expr_to_str depth flatten e)
    | StmtPrint e ->
        sprintf "print(%s);"
            (expr_to_str depth flatten e)
    | StmtBlock block ->
        if flatten then
            sprintf "{%s}" (String.concat " " (List.map (decl_to_str 0 false) block))
        else
            sprintf "{\n%s\n%s}"
                (String.concat "\n" (List.map (fun d -> sprintf "%s%s" (whitespace depth) (decl_to_str depth flatten d)) block))
                (whitespace (depth - 1))
    | StmtExpr e ->
        sprintf "%s;"
            (expr_to_str depth flatten e)

and expr_to_str ?(use_paren=false) depth flatten expr =
    match expr with
    | ExprAssign (e1, e2) ->
        sprintf "%s = %s"
            (expr_to_str depth flatten e1) (expr_to_str depth flatten e2)
    | ExprBinary (op, e1, e2) ->
        sprintf (if use_paren then "(%s %s %s)" else "%s %s %s")
            (expr_to_str ~use_paren:true depth flatten e1) (bop_to_str op) (expr_to_str ~use_paren:true depth flatten e2)
    | ExprUnary (op, e) ->
        sprintf "%s%s"
            (uop_to_str op) (expr_to_str depth flatten e)
    | ExprCall (e, args) ->
        sprintf "%s(%s)"
            (expr_to_str depth flatten e) (String.concat ", " (List.map (expr_to_str depth flatten) args))
    | ExprSelect (e, ids) ->
        sprintf "%s.%s"
            (expr_to_str depth flatten e) (String.concat "." ids)
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
    | OpEq   -> "=="
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

let to_str prog =
    String.concat "\n" (List.map (decl_to_str 0 false) prog) ^ "\n"

end (* Ast *)
