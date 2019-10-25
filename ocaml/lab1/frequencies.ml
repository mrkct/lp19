open Printf


let rec string_filter f s =
  if String.length s = 0 then
    ""
  else
    let l = (String.length s) - 1 in
    let rest = (string_filter f (String.sub s 1 l)) in
    if f s.[0] then
      (String.make 1 s.[0]) ^ rest
    else
      rest



let remove_punctuation = string_filter (fun c -> 
  match c with
  | ',' | '.' | ';' | ':' | '"' -> false
  | _ -> true
) ;;

let () =
  let filename = "sample.txt" in
  let file = open_in filename in
  let filesize = in_channel_length file in
  let freq = Hashtbl.create 1000 in
  while pos_in file < filesize do
    input_line file 
    |> String.split_on_char ' '
    |> List.filter (fun x -> x <> "")
    |> List.map String.lowercase_ascii
    |> List.map remove_punctuation
    |> List.iter (fun word ->  
      if Hashtbl.mem freq word then
        Hashtbl.replace freq word (1 + (Hashtbl.find freq word))
      else
        Hashtbl.add freq word 1
    )
  done ; 
  close_in_noerr file ; 
  Hashtbl.iter (fun key value -> printf "%s : %d\n" key value) freq