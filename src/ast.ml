open Printf

type expr =
    (* values *)
    | ENull
    | ETrue
    | EFalse
    (* variables *)
    | EIdent of string
    | EInt of int
    | EFloat of float
    (* expressions *)
    | EBlock of expr list
    | ECall of expr * expr list
    | ESelect of expr * string list
    | EFunc of string * string list * expr
    | EVar of string * expr
    | EAssign of string * expr
    | ECond of expr * expr * expr
    | EPrint of expr
    | EBinary of bop * expr * expr
    | EUnary of uop * expr

and bop =
    | OpEq
    | OpNe
    | OpLt
    | OpLe
    | OpGt
    | OpGe
    | OpAnd
    | OpOr
    | OpPlus
    | OpMin
    | OpMult
    | OpDiv
    | OpMod

and uop =
    | OpNeg
    | OpNot

let bop_to_str op =
    match op with
    | OpEq -> "="
    | OpNe -> "!="
    | OpLt -> "<"
    | OpLe -> "<="
    | OpGt -> ">"
    | OpGe -> ">="
    | OpAnd -> "&&"
    | OpOr -> "||"
    | OpPlus -> "+"
    | OpMin -> "-"
    | OpMult -> "*"
    | OpDiv -> "/"
    | OpMod -> "%"

let uop_to_str op =
    match op with
    | OpNeg -> "-"
    | OpNot -> "!"

let rec expr_to_str expr =
    match expr with
    (* values *)
    | ENull -> "null"
    | ETrue -> "true"
    | EFalse -> "false"
    (* variables *)
    | EIdent s -> s
    | EInt x -> string_of_int x
    | EFloat x -> string_of_float x
    (* expressions *)
    | EBlock exprs ->
        sprintf "{%s;}" (String.concat "; " (List.map expr_to_str exprs))
    | ECall (e, args) ->
        sprintf "%s(%s)" (expr_to_str e) (String.concat ", " (List.map expr_to_str args))
    | ESelect (e, ids) ->
        sprintf "%s.%s" (expr_to_str e) (String.concat "." ids)
    | EFunc (name, args, e) ->
        sprintf "function %s(%s) %s" name (String.concat ", " args) (expr_to_str e)
    | EVar (s, e) -> sprintf "var %s = %s" s (expr_to_str e)
    | EAssign (s, e) -> sprintf "%s = %s" s (expr_to_str e)
    | ECond (e1, e2, e3) ->
        sprintf "if %s then %s else %s" (expr_to_str e1) (expr_to_str e2) (expr_to_str e3)
    | EPrint e -> sprintf "print(%s)" (expr_to_str e)
    | EBinary (op, e1, e2) ->
        sprintf "(%s %s %s)" (expr_to_str e1) (bop_to_str op) (expr_to_str e2)
    | EUnary (op, e) ->
        sprintf "%s%s" (uop_to_str op) (expr_to_str e)
