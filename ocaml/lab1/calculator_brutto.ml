#load "str.cma" ;;


module Calculator : sig 
  type token
  type expr
  
  val string_to_expr: string -> expr
  val eval: expr -> int
end = struct
  type operator = Add | Sub | Mul | Div 
  type token = 
    | Operand of int
    | Operator of operator
  type expr = Expr of token list
  
  let to_token s =
    match s with
    | "+" -> Operator Add 
    | "-" -> Operator Sub
    | "*" -> Operator Mul
    | "/" -> Operator Div
    | _ -> Operand (int_of_string s)

  let string_to_expr s = 
    let s = Str.split (Str.regexp " +")  s in
    Expr ( List.map to_token s )
  
  let eval exp =
    let stack = Stack.create () in
    let rec aux ls =
      match ls with
      | [] -> Stack.pop stack 
      | (Operand x) :: xs -> Stack.push x stack ; aux xs 
      | (Operator x) :: xs->
        match x with
        | Add -> Stack.push ((Stack.pop stack) + (Stack.pop stack)) stack ; aux xs
        | Sub -> Stack.push ((Stack.pop stack) - (Stack.pop stack)) stack ; aux xs
        | Mul -> Stack.push ((Stack.pop stack) * (Stack.pop stack)) stack ; aux xs
        | Div -> Stack.push ((Stack.pop stack) / (Stack.pop stack)) stack ; aux xs
      in
    match exp with
    | Expr (ls) -> aux ls
end

let test () =
  let t = [
    "1 1 +";
    "1 1 -";
    "1 1 *";
    "1 1 /";
    "1 1 + 1 + 1 +";
    "6 5 * 10 /";
    "2 2 + 4 * 2 - 7 /"
  ] in
  List.iter (fun t -> printf "%s = %d\n" t (Calculator.eval (Calculator.string_to_expr t))) t