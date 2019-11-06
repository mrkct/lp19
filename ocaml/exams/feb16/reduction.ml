module ArithExpr : sig
  val print_evaluation : string -> unit
end = struct
  type expr =
    | Val of float
    | Sum of expr * expr
    | Sub of expr * expr
    | Div of expr * expr
    | Mul of expr * expr
  
  let string_to_expr e = 
    let s = Stack.create () in
    for i=(String.length e) -1 downto 0 do
      match String.get e i with
      | '+' -> 
        let a = Stack.pop s and b = Stack.pop s in
        Stack.push (Sum (a, b)) s
      | '-' -> 
        let a = Stack.pop s and b = Stack.pop s in
        Stack.push (Sub (a, b)) s
      | '*' -> 
        let a = Stack.pop s and b = Stack.pop s in
        Stack.push (Mul (a, b)) s
      | '/' -> 
        let a = Stack.pop s and b = Stack.pop s in
        Stack.push (Div (a, b)) s
      | x   -> Stack.push (Val (float_of_string (String.make 1 x))) s
    done ; Stack.pop s
  
  let rec print_expr = function
    | Val x -> string_of_float x
    | Sum(a, b) -> "( " ^ (print_expr a) ^ " + " ^ (print_expr b) ^ " )"
    | Sub(a, b) -> "( " ^ (print_expr a) ^ " - " ^ (print_expr b) ^ " )"
    | Mul(a, b) -> "( " ^ (print_expr a) ^ " * " ^ (print_expr b) ^ " )"
    | Div(a, b) -> "( " ^ (print_expr a) ^ " / " ^ (print_expr b) ^ " )"

  let rec eval_step = function
    | Sum(Val a, Val b) -> Val (a +. b)
    | Sub(Val a, Val b) -> Val (a -. b)
    | Mul(Val a, Val b) -> Val (a *. b)
    | Div(Val a, Val b) -> Val (a /. b)
    | Sum(a, b)         -> Sum (eval_step a, eval_step b)
    | Sub(a, b)         -> Sub (eval_step a, eval_step b)
    | Mul(a, b)         -> Mul (eval_step a, eval_step b)
    | Div(a, b)         -> Div (eval_step a, eval_step b)
    | Val x             -> Val x
  
  let print_evaluation e = 
    let rec aux = function
    | Val x -> 
      Printf.printf "%s\n" (print_expr (Val x))
    | e     -> 
      Printf.printf "%s\n" (print_expr e); 
      aux (eval_step e)
    in aux (string_to_expr e)
end