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


let rec length(l: 'a list): int = 
  match l with 
  | [] -> 0
  | _ :: tail -> 1 + length tail

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
  

let rec debug_print_list(l: 'a list) (stringify: 'a -> string): unit = 
  match l with 
  | [] -> ()
  | head :: tail -> 
    let str = (stringify head) in 
     (print_endline str); debug_print_list tail stringify


(* this is FIFO, can swap the reduce and accumulator call order to reverse it *)
let rec reduce (identity: 'a) (accumulator: 'a -> 'b -> 'a) (list: 'b list): 'a = 
  match list with
  | [] -> identity
  | (head: 'b) :: (tail: 'b list) -> reduce (accumulator identity head) accumulator tail


(* Adds all the variables in new_var_list to old_env, they should shadow/override any old definitions *)
let add_all_to_env (old_env: env) (eval_fn: Untyped.expr -> env -> Value.t) (new_var_list: Untyped.binding list): env = 
  let accumulator = fun (inner_env: env) (var: Untyped.binding): env -> 
      (let (var_name, var_value) = var in env_update var_name (eval_fn var_value old_env) inner_env) 
      in 
      (reduce old_env (accumulator) new_var_list) 



(* Evaluate an expression in a given environment.  EvalExn on errors. *)
let rec eval_expr (e: Untyped.expr) (env: env): Value.t =
  match e with 
  | Float f -> Float (f)
  | BinOp (op, e1, e2) -> (
    let left = eval_expr e1 env in 
    let right = eval_expr e2 env in 
  
    match left, right with 
    | Float lf, Float rf -> Float (eval_binop op lf rf)
    | _, _ -> raise (EvalExn "Binary operation on non-float values")
  )

  | If0 (con, yes_b, no_b) -> (
    let cond_res = ((get_float (eval_expr con env)) = 0.) in
    if cond_res then eval_expr yes_b env else eval_expr no_b env
  )


  | Let (var_list, expr) -> (
    match (find_duplicate var_list) with 
    | Some dupe -> (raise (EvalExn ("Duplicate identifier: " ^ (let (ident, _) = dupe in (let Ident ident_name = ident in (ident_name))))))
    | None -> (
      let new_env = add_all_to_env env (eval_expr) var_list
      in 
      (eval_expr expr new_env)

    )
  )

  | Var var -> (
    match (env_lookup var env) with 
    | Some expr_inner -> expr_inner
    | None -> raise (EvalExn ("Undefined variable: " ^ (let Ident var_name = var in var_name)))
  )

  | Fun (args, body) -> (
    Closure (args, body, env) (* todo dynamic scope bad!*)
  )



  | App (the_fun, args) -> (
    match (eval_expr the_fun env) with 
    | Closure (params, body, closure_env) -> (
      let args_length = (length args) in
      let params_length = (length params) in
      if (args_length <> params_length) then 
        (raise (EvalExn ("Function expected " ^ (string_of_int params_length) ^ " arguments but got " ^ (string_of_int args_length)))) 
      else (
        (args_length |> string_of_int |> print_endline);
        (params_length |> string_of_int |> print_endline); 
        (debug_print_list params (fun head -> let (Ident s) = head in s));
        ();

        Float 5578.

      )


    )
    | _ -> (raise (EvalExn "Attempting to call a non-function value") )
  )

let eval (p: Untyped.prog) : Value.t Value.result =
  try Value (eval_expr p EmptyEnv)
  with EvalExn msg -> ValueError msg
