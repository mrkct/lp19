let rec last_two = function
  | x::y::[] -> Some (x, y)
  | [] -> None
  | [x] -> None
  | x::xs -> last_two xs ;;