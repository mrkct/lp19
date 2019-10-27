let rec is_palindrome s =
  let l = String.length s in
  if l <= 1 then
    true
  else 
    s.[0] = s.[l-1] && is_palindrome (String.sub s 1 (l-2))

let rec (-) a b =
  let rec string_filter f s =
    if String.length s = 0 then
      ""
    else
      let l = (String.length s) - 1 in
      let rest = (string_filter f (String.sub s 1 l)) in
      if f s.[0] then
        (String.make 1 s.[0]) ^ rest
      else
        rest in
  string_filter (fun c -> not (String.contains b c)) a

let anagram s dictionary = 
  let rec string_to_list s =
    if s = "" then 
      [] 
    else 
      let rest = String.sub s 1 ((String.length s) - 1) in
      s.[0] :: string_to_list rest in
  let list_to_string l = 
    List.fold_left (fun acc c -> acc ^ (String.make 1 c)) "" l in
  let sort_string s =
    list_to_string (List.sort compare (string_to_list s)) in
  let sorted_s = sort_string s in
    List.exists (fun w -> sorted_s = (sort_string w)) dictionary ;;