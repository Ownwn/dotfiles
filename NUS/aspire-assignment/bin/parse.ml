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


let str_of_ident (id: Ident.t): string = 
  (match id with | Ident s -> s)

let rec check_identifiers (bindings: Ident.t list) : unit = (* todo need to use this actually!*)
  match find_duplicate bindings with | None -> () | _ -> (* todo can i use if expr instead of match to grab Option? *)
    failwith "todo error name here"

let parse_let_binding (parse_expr: Sexp.t -> expr) (bindings: Sexp.t list): expr = 
  match bindings with 
  | [List def_pairs; body] -> (
    let binding_mapper: Token.token -> binding = fun tok -> (
      match tok with 
      | List [Atom identifier; value] -> ((Ident (identifier)), (parse_expr value)) 
      | _ -> (failwith "todo wrong list in let body!")
    ) in
    
    let bind_list: binding list = (List.map binding_mapper def_pairs) in 
    
    (* this line not working *)
    let duplicate_bind_option: string option = (find_duplicate (List.map ((fun b -> (let (var_identifier, _) = b in str_of_ident var_identifier)): binding -> string ) bind_list)) in
    
    match duplicate_bind_option with 
    | Some dupe -> (raise (ParseExn ("Duplicate identifier: " ^ dupe)))
    | None -> Let (bind_list, parse_expr body)
    
  
  )
  | _ -> raise (ParseExn "Invalid let format")


let parse_fun_declaration (parse_expr: Sexp.t -> expr) (function_decal: Sexp.t list): expr = 
  match function_decal with 
  | [List args; body] -> (
    let arg_mapper: Sexp.t -> Ident.t = fun tok -> (
      match tok with 
      | Atom a -> (
        match make_ident a with 
        | Some id -> id
        | None -> (raise (ParseExn ("Invalid identifier in function parameter:" ^ a)))
      (* todo this indents is cooked! *)
      )
      | _ -> (failwith "todo bad here")

    ) in 

    let ident_list: Ident.t list = (List.map arg_mapper args) in
    Fun (ident_list, parse_expr body)
  
  )

  | _ -> raise (ParseExn "Invalid fun format")

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
      | None -> raise (ParseExn ("Invalid identifier in variable reference: " ^ a))
    )
  )

  | List (Atom head :: tail) -> (
    match parse_binop head with 
    | Some bin -> (
      match tail with 
      | [op1; op2] -> (BinOp (bin, parse_expr op1, parse_expr op2))
      | _ -> raise (ParseExn "Invalid binary operation format")
    )
    | None -> (
      match head with 
      | "if0" -> (
        match tail with 
        | [con; yes_b; no_b] -> (If0 (parse_expr con, parse_expr yes_b, parse_expr no_b))
        | _ -> raise (ParseExn "Invalid if0 format")
      )

      | "let" ->  (
        parse_let_binding (parse_expr) (tail)
      )

      | "fun" -> (
        parse_fun_declaration (parse_expr) (tail)
      )

      | bad_var -> print_endline bad_var; (failwith "other bad parse todo")

    
    )
  )

  | List (head :: tail) -> (
    failwith "nah bad :("
  )

(* 
  | bob -> failwith ("rip: " ^ (match bob with 
  | Atom hh -> ("atom and " ^ hh)
  | nah -> ("other one: " ^ Sexp.to_string nah)
  
  
  )) *)

  | _ -> raise (ParseExn "Null expression")

  

  (* if (not (List.for_all (fun bb -> (print_endline (Sexp.to_string bb)); true) tail)) then (failwith "nahahah") else *)
  


(* Parse an expression representing a binding  *)
and parse_binding (sexp: Sexp.t) : binding =
  failwith (__LOC__ ^ ": unimplemented")

let parse (tokens : Token.t Pos.t list) : prog =
  match tokens with
  | [sexp, _, _] -> parse_expr sexp (* Ignore Pos.t and list, they are framework artifacts *)
  | _ -> raise (ParseExn "Expected a single S-expression for the program")
