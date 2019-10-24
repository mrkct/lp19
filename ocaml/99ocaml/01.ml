let rec last = function
| x::[] -> Some x
| x::xs -> last xs 
| _ -> None ;;

last [1; 2; 3; 4] ;;