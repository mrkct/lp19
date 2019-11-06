
let log2 x =
  let xf = float_of_int x in
  int_of_float ((log10 xf) /. (log10 2.))

let ipow x p = int_of_float ((float_of_int x) ** (float_of_int p) )

let trialdivision x = 
  let rec aux i =
    if x mod i = 0 then false else i >= (x/2) || aux (i+1)
  in aux 2

let lucaslehmer x =
  let p = log2 (x + 1) in
  let rec s i =
    if i = 0 then 4 else (ipow (s (i-1)) 2) - 2
  in ((s (p-2)) mod x) = 0

let littlefermat x = 
  let testcases = 100 in
  let rec randlist n = 
    if n = 0 then [] else (int_of_float (Random.float (float_of_int x))) :: (randlist (n-1))
  in List.for_all (fun y -> ((ipow y (x-1)) mod x) = 0 ) (randlist testcases)

let is_prime = function
  | x when x <= 10000  -> trialdivision x
  | x when x <= 524287 -> lucaslehmer x
  | x -> littlefermat x