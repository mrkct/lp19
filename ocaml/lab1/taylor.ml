open Printf ;;


let fact x =
  if x <= 0 then 
    1
  else
    let rec aux acc n =
      if n = x then n*acc else aux (acc*n) (n+1) in
    aux 1 1

let rec mysin x n =
  let term x n = 
    let i = 2. *. (float_of_int n) +. 1. in
    let v = (x ** i) /. (float_of_int (fact (int_of_float i))) in 
    if n mod 2 = 0 then v else (v *. (-1.))
  in
  if n = 0 then
    term x 0
  else
    (term x n) +. (mysin x (n-1))