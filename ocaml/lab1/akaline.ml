(*
Exercise 1: A few of Chemistry.

Put into a list, called alkaline_earth_metals, the atomic numbers of the six alkaline earth metals: 
beryllium (4), magnesium (12), calcium (20), strontium (38), barium (56), and radium (88). 
Then

    Write a function that returns the highest atomic number in alkaline_earth_metals.
    Write a function that sorts alkaline_earth_metals in ascending order (from the lightest to the heaviest).

Put into a second list, called noble_gases, the noble gases: helium (2), neon (10), argon (18), krypton (36), xenon (54), and radon (86). Then

    Write a function (or a group of functions) that merges the two lists and print the result as couples (name, atomic number) sorted in ascending order on the element names.
 
*)

open Printf ;;

let alkaline_earth_metals = [
  (4, "beryllium"); 
  (12, "magnesium"); 
  (20, "calcium");
  (28, "strontium");
  (56, "barium");
  (88, "radium")
] ;;

let alkaline_earth_metals2 = [
  (88, "radium");
  (56, "barium");
  (12, "magnesium"); 
  (20, "calcium");
  (4, "beryllium");
  (28, "strontium");
] ;;

let highest ls =
  let rec aux h ls =
    match ls with
    | [] -> h
    | x::xs -> if x > h then aux x xs else aux h xs in
  aux (List.nth ls 0) ls ;;

let sort ls = 
  let cmp a b = 
    match (a, b) with
    | ((x, _), (y, _)) when x > y -> 1
    | ((x, _), (y, _)) when x < y -> -1
    | _ -> 0
  in List.sort cmp ls ;;

let noble_gases = [
  (2, "helium");
  (10, "neon");
  (18, "argon");
  (36, "krypton");
  (54, "xenon");
  (86, "radon");
] ;;

let rec merge a b =
  let cmp a b = 
    match (a, b) with
    | ((_, a), (_, b)) when a > b -> 1
    | ((_, a), (_, b)) when a < b -> -1
    | _ -> 0 in
  let result = List.sort cmp (a @ b) in
  let rec print = function
  | (id, name)::xs -> 
    printf "(%d, %s) " id name;
    print xs
  | [] -> printf "\n" in
  print result ;;
  