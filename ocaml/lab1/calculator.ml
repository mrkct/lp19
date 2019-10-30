#load "str.cma"
open Printf


module Calculator : sig
  type expr

  exception InvalidExpression

  val string_to_expr: string -> expr
  val eval: expr -> int
end = struct
  exception InvalidExpression

  type expr =
    | Val of int
    | Sum of expr * expr
    | Sub of expr * expr
    | Div of expr * expr
    | Mul of expr * expr
    | Pow of expr * expr
  
  let string_to_expr s = 
    let stack = Stack.create () in
    let handle_token = function
      | "+" -> Stack.push (Sum (Stack.pop stack, Stack.pop stack)) stack
      | "-" -> Stack.push (Sub (Stack.pop stack, Stack.pop stack)) stack
      | "*" -> Stack.push (Mul (Stack.pop stack, Stack.pop stack)) stack
      | "/" -> Stack.push (Div (Stack.pop stack, Stack.pop stack)) stack
      | "^" -> Stack.push (Pow (Stack.pop stack, Stack.pop stack)) stack
      | x   -> Stack.push (Val (int_of_string x)) stack
    in
    List.iter handle_token (Str.split (Str.regexp " +") s) ;
    if Stack.length stack = 1 then Stack.pop stack else raise InvalidExpression
  
  let rec eval = function
    | Val x       -> x
    | Sum (a, b)  -> (eval a) + (eval b)
    | Sub (a, b)  -> (eval a) - (eval b)
    | Mul (a, b)  -> (eval a) * (eval b)
    | Div (a, b)  -> (eval a) / (eval b)
    | Pow (a, b)  -> int_of_float ((float_of_int (eval a)) ** (float_of_int (eval b))) 
end