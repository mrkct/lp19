open Printf


let testSize = 100

let rec range start finish =
  if start = finish then [] else start :: range (start+1) finish


module Monoid : sig
  val verify: 'a list -> ('a -> 'a -> 'a) -> 'a -> bool
end = struct

  let test_identity set op id =
    List.for_all (fun x -> (op x id) = x) set
  
  let test_associativity set op =
    List.for_all (fun x -> 
      List.for_all (fun y -> 
        List.for_all (fun z -> (op x (op y z) = (op (op x y) z))) set
      ) set
    ) set
  
  (*
    Testa che per il risultato di qualunque operazione rimanga nel set
    Non sapevo come chiamarla
  *)
  let test_asd set op =
    List.for_all (fun x ->
      List.for_all (fun y ->
        List.exists (fun z -> z = (op x y)) set
      ) set
    ) set

  let verify set op id =
    (test_identity set op id) && (test_associativity set op) && (test_asd set op)
end


let () =
  let sample_set = (range 0 testSize) in
  printf "Set numeri da 0 a %d:\n" testSize ;
  printf "Operazione (x+y) mod %d\n" testSize ;
  printf "E' un monoide: %b\n" (Monoid.verify sample_set (fun x y -> (x + y) mod testSize) 0) ;

  printf "Operazione x-y\n" ;
  printf "E' un monoide: %b\n" (Monoid.verify sample_set (-) 0)
