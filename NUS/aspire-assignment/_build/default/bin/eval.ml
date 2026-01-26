open Aspire
open Aspire.Value

(* NOTE: Some of these functions should raise an EvalExn for some inputs.  No
other exceptions should be raised. *)
exception EvalExn of string

let env_update (name: Ident.t) (value: Value.t) (env: env): env =
  ConsEnv (name, value, env)

let rec env_lookup (name: Ident.t) (env: env): Value.t option =
  match env with
  | EmptyEnv -> None
  | ConsEnv (k, v, rest) -> if (k = name) then Some v else env_lookup name rest
  
(* Evaluate a binop applied to two floats.  May raise EvalExn on division by
zero.

Hint: For compaing a float with zero, use `=`. *)
let eval_binop (op: Untyped.binop) (x: float) (y: float): float =
  match op with
  | Add -> x +. y
  | Mul -> x *. y
  | Div -> (if (not (y = 0.)) then x /. y else raise (EvalExn "Division by zero"))
  | Sub -> x -. y
  
  
  (* todo delme *)
  (* echo "(+ 1 2)" | dune exec -- aspire test owen-tests --detail --ref=all *)



let rec contains (x: 'a) (l: 'a list): bool =
  match l with
  | [] -> false
  | head :: tail -> (head = x) || contains x tail


let rec find_duplicate (l: 'a list): 'a option = 
  match l with
  | [] -> None
  | head :: tail -> if (contains head tail) then (Some head) else (find_duplicate tail)


let get_float (t: t): float = 
  match t with 
  | Float f -> f
  | _ -> failwith "Expected float when evaluating expr!"
  


(* I'm sure the order of operations here is totally mangled, but it shouldn't matter since we don't allow env name shadowing (duplicate identifiers) :) *)
let rec reduce (identity: 'a) (accumulator: 'a -> 'b -> 'a) (list: 'b list): 'a = 
  match list with
  | [] -> identity
  | (head: 'b) :: (tail: 'b list) -> reduce (accumulator identity head) accumulator tail

(* Evaluate an expression in a given environment.  EvalExn on errors. *)
let rec eval_expr (e: Untyped.expr) (env: env): Value.t =
  match e with 
  | Float f -> Float (f)
  | BinOp (op, e1, e2) -> (
    let left = eval_expr e1 env in 
    let right = eval_expr e2 env in 
  
    match left, right with 
    | Float lf, Float rf -> Float (eval_binop op lf rf)
    | _, _ -> raise (EvalExn "Expecting floats for binary operator")
  )

  | If0 (con, yes_b, no_b) -> (
    let cond_res = ((get_float (eval_expr con env)) = 0.) in
    if cond_res then eval_expr yes_b env else eval_expr no_b env
  )


  | Let (var_list, expr) -> (
    match (find_duplicate var_list) with 
    | Some dupe -> (raise (EvalExn ("Duplicate identifier: " ^ (let (ident, _) = dupe in (let Ident ident_name = ident in (ident_name))))))
    | None -> (
      
      let new_env = (reduce env (fun iden acum -> iden) var_list) in 
      (eval_expr expr new_env)

    )
  )

  | Var var -> (
    match (env_lookup var env) with 
    | Some expr_inner -> expr_inner
    | None -> raise (EvalExn ("Undefined variable: " ^ (let Ident var_name = var in var_name)))
  )
  
  
  
  

  | _ -> failwith "NYI eval other case"

let eval (p: Untyped.prog) : Value.t Value.result =
  try Value (eval_expr p EmptyEnv)
  with EvalExn msg -> ValueError msg
