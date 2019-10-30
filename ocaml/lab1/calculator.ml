#load "str.cma"
open Printf


module type StackADT = sig
  type 'a stack

  val create: unit -> 'a stack
  val push: 'a -> 'a stack -> unit
  val pop: 'a stack -> 'a
  val length: 'a stack -> int
end

module SimpleStack : StackADT = struct
  type 'a stack = {mutable stack: 'a list}

  let create () = {stack=[]}
  
  let push x s = 
    s.stack <- (x::s.stack)
  
  let pop s =
    match s.stack with
    | [] -> failwith "stack is empty"
    | x :: xs -> (s.stack <- xs ) ; x
  
  let length s = 
    List.length s.stack
end

module LimitedStack : StackADT = struct
  type 'a stack = {mutable stack: 'a list}

  let create () = {stack=[]}
  
  let push x s = 
    if List.length s.stack < 5 then
      s.stack <- (x::s.stack)
    else failwith "max 5 elements allowed"
  
  let pop s =
    match s.stack with
    | [] -> failwith "stack is empty"
    | x :: xs -> (s.stack <- xs ) ; x
  
  let length s = 
    List.length s.stack
end

module type CalculatorADT = sig
  type expr

  exception InvalidExpression

  val string_to_expr: string -> expr
  val expr_to_string: expr -> string
  val eval: expr -> int
end

module Calculator (Stack: StackADT) : CalculatorADT = struct
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
  
  let rec expr_to_string = function
    | Val x       -> string_of_int x
    | Sum (a, b)  -> "(" ^ (expr_to_string a) ^ " + " ^ (expr_to_string b) ^ ")"
    | Sub (a, b)  -> "(" ^ (expr_to_string a) ^ " - " ^ (expr_to_string b) ^ ")"
    | Mul (a, b)  -> "(" ^ (expr_to_string a) ^ " * " ^ (expr_to_string b) ^ ")"
    | Div (a, b)  -> "(" ^ (expr_to_string a) ^ " / " ^ (expr_to_string b) ^ ")"
    | Pow (a, b)  -> "(" ^ (expr_to_string a) ^ " ** " ^ (expr_to_string b) ^ ")"
  
  let rec eval = function
    | Val x       -> x
    | Sum (a, b)  -> (eval a) + (eval b)
    | Sub (a, b)  -> (eval a) - (eval b)
    | Mul (a, b)  -> (eval a) * (eval b)
    | Div (a, b)  -> (eval a) / (eval b)
    | Pow (a, b)  -> int_of_float ((float_of_int (eval a)) ** (float_of_int (eval b))) 
end

module MyCalc = Calculator (SimpleStack)

let () =
  let sample = MyCalc.string_to_expr "3 5 + 6 * 12 /"
  in
  printf "%s\n" (MyCalc.expr_to_string sample)