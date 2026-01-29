open Sexplib
open Aspire
open Aspire.Surface


let rec contains (x: 'a) (l: 'a list): bool =
  match l with
  | [] -> false
  | head :: tail -> (head = x) || contains x tail

let rec find_duplicate (l: 'a list): 'a option = 
  match l with
  | [] -> None
  | head :: tail -> if (contains head tail) then (Some head) else (find_duplicate tail)


(* NOTE: Some of these functions may raise a ParseExn.  No other exceptions
should be raised. *)

(* Convert an operator name to a `binop` *)
let parse_binop (s: string) : binop option = (* todo I have modified this to be option, previously it wasnt option. *)
  match s with 
  | "+" -> Some Add
  | "-" -> Some Sub
  | "*" -> Some Mul
  | "/" -> Some Div
  | _ -> None

let make_ident (s: string) : Ident.t option =
  match float_of_string_opt s with
  | Some _ -> None
  | None ->
    if List.mem s ["+"; "-"; "*"; "/"; "let"; "if0"; "fun"]
    then None
    else Some (Ident.Ident s)


let rec check_identifiers (bindings: Ident.t list) : unit = (* todo need to use this actually!*)
  match find_duplicate bindings with | None -> () | _ -> (* todo can i use if expr instead of match to grab Option? *)
    failwith "todo error name here"


(* Parse an s-expression into an expr.

NOTE: use float_of_string_opt to distinguish an Atom that should be a float from
one that should be an identifier, then use make_ident to make identifiers *)
let rec parse_expr (sexp: Sexp.t) : expr =
  match sexp with 
  | Atom a -> (
    match float_of_string_opt a with 
    | Some f -> Float f
    | None -> (
      match make_ident a with 
      | Some ident -> Var ident
      | None -> failwith "todo checkme here aa"
    )
  )

  | List (Atom head :: tail) -> (
    match parse_binop head with 
    | Some bin -> (
      match tail with 
      | [op1; op2] -> (BinOp (bin, parse_expr op1, parse_expr op2))
      | _ -> (failwith "tood bad num of operands")
    )
    | None -> (
      match head with 
      | "if0" -> (
        match tail with 
        | [con; yes_b; no_b] -> (If0 (parse_expr con, parse_expr yes_b, parse_expr no_b))
        | _ -> failwith "Bad If0 operand"
      )

      | "let" ->  (
        let let_fail = fun () -> (raise (ParseExn "Invalid let format")) in

        match tail with 
        | [defs; body] -> (
          
        )
        | _ -> let_fail ()
      )

      | _ -> (failwith "other bad parse todo")

    
    )
  )

  | _ -> failwith "parse NYI"

  

  
  


(* Parse an expression representing a binding  *)
and parse_binding (sexp: Sexp.t) : binding =
  failwith (__LOC__ ^ ": unimplemented")

let parse (tokens : Token.t Pos.t list) : prog =
  match tokens with
  | [sexp, _, _] -> parse_expr sexp (* Ignore Pos.t and list, they are framework artifacts *)
  | _ -> raise (ParseExn "Expected a single S-expression for the program")
