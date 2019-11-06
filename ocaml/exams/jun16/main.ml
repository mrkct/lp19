open Qsort
open Cqsort


let make_list x =
  let rec make_list x acc =
    if x = 0 then acc
    else make_list (x-1) (x::acc)
  in make_list x []

let profile f =
  let s = Sys.time() in
    let _ = f() in (Sys.time()) -. s

let main () =
  let l = make_list 10000 in
  Printf.printf
    "Sorting 10000 elements with the functional qsort takes %3.2f μs\n"
    (profile (fun () -> qsort (>) l));
  Printf.printf 
    "Sorting 10000 elements with the continuation qsort takes %3.2f μs\n"
    (profile (fun () -> cqsort (>) l))
  
let() = main()