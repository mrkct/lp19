let rec rev = function
  | [] -> []
  | x::xs -> (rev xs) @ [x] ;;

let rec rev2 ls =
  let rec aux acc = function 
    | [] -> acc
    | x::xs -> aux (x :: acc) xs in
  aux [] ls ;; 