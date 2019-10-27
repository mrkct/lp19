let is_prime x =
  let rec aux n =
    if n > (x/2) then
      true
    else
      if x mod n = 0 then false else aux (n+1)
  in aux 2

let rec all_primes n =
  if n = 1 then 
    [] 
  else
    if is_prime n then n :: (all_primes (n-1)) else all_primes (n-1) 
  
let goldbach_list n =
  List.map (fun x -> (x, n-x)) (
    List.filter (fun x -> is_prime (n-x)) (all_primes n)
  )

let goldbach n = 
  match goldbach_list n with
  | x::xs -> x
  | _ -> failwith "nobel per la matematica"