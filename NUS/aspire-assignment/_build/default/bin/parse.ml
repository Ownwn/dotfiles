open Sexplib
open Aspire
open Aspire.Surface


(* NOTE: Some of these functions may raise a ParseExn.  No other exceptions
should be raised. *)

(* Convert an operator name to a `binop` *)
let parse_binop (s: string) : binop =
  failwith (__LOC__ ^ ": unimplemented")

let make_ident (s: string) : Ident.t option =
  match float_of_string_opt s with
  | Some _ -> None
  | None ->
    if List.mem s ["+"; "-"; "*"; "/"; "let"; "if0"; "fun"]
    then None
    else Some (Ident.Ident s)


(* Raise a ParseExn if any of the idneitifiers are duplicates of each other.

Hint: Use List.exists and recursion *)
let rec check_identifiers (bindings: Ident.t list) : unit =
  failwith (__LOC__ ^ ": unimplemented")


(* Parse an s-expression into an expr.

NOTE: use float_of_string_opt to distinguish an Atom that should be a float from
one that should be an identifier, then use make_ident to make identifiers *)
let rec parse_expr (sexp: Sexp.t) : expr =
  match sexp with 
  | Atom a -> (Float (float_of_string a))
  | List (Atom "+" :: tail) -> (
    match tail with 
    | [op1; op2] -> (BinOp (Add, parse_expr op1, parse_expr op2))
    | _ -> failwith "Bad Add operand"
  ) 
  | List (Atom "if0" :: tail) -> (
    match tail with 
    | [con; yes_b; no_b] -> (If0 (parse_expr con, parse_expr yes_b, parse_expr no_b))
    | _ -> failwith "Bad If0 operand"
  )
  | _ -> failwith "parse NYI"


(* Parse an expression representing a binding  *)
and parse_binding (sexp: Sexp.t) : binding =
  failwith (__LOC__ ^ ": unimplemented")

let parse (tokens : Token.t Pos.t list) : prog =
  match tokens with
  | [sexp, _, _] -> parse_expr sexp (* Ignore Pos.t and list, they are framework artifacts *)
  | _ -> raise (ParseExn "Expected a single S-expression for the program")
