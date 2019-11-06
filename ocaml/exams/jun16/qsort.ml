let qsort (>:) l =
  let rec qsort = function
  | [] -> []
  | h::tl -> (qsort (List.filter (fun x -> (x >: h)) tl))
              @ [h] @
              (qsort (List.filter (fun x -> (h >: x)) tl))
  in qsort l