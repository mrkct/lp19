let cqsort (>:) (ls: int list) =
  let rec qsort l cont =
    match l with
    | [] | [_] -> cont l
    | x::xs -> 
      let left  = List.filter (fun y -> (x >: y)) xs in
      let right = List.filter (fun y -> (y >: x)) xs in
      qsort left (fun sl -> 
        cont (qsort right (fun sr -> sl @ (x::sr)))
      )
  in qsort ls (fun sorted -> sorted)
