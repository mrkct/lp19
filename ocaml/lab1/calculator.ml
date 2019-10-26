module PolishCalculator : sig
  type expr 
  val expr_of_string: string -> expr
  val eval: expr -> int
end = 
struct
  type value = Operator of char | Operand of int 
  type expr = Expr of string list
  let expr_of_string s = 
    Expr (String.split_on_char ' ' s)

  
  let eval exp =

end