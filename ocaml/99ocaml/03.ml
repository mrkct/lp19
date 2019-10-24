let rec at pos = function
  | x::_ when pos = 0 -> Some x
  | _::xs when pos > 0 -> at (pos-1) xs
  | _ -> None ;;