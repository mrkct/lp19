(*
  Beyond the well-known Celsius and Fahrenheit, there are other 
  six temperature scales: 
  Kelvin, Rankine, Delisle, Newton, Réaumur, and Rømer 
  (Look at 
  http://en.wikipedia.org/wiki/Comparison_of_temperature_scales to read about them).

    Write a function that given a pure number prints 
    a conversion table for it among any of the 8 scales 
    (remember that functions are high-order).
    
    Write a function that given a temperature in a specified scale
     returns a list of all the corresponding temperatures in 
     the other scales, note that the scale must be specified (hint: use a tuple).

*)

type scale = 
  | Celsius 
  | Fahrenheit 
  | Kelvin 
  | Rankine 
  | Delisle 
  | Newton 
  | Reamur 
  | Romer ;;

let toKelvin degree = function
  | Celsius -> degree +. 273.15
  | Fahrenheit -> (degree +. 459.67) *. (5. /. 9.)
  | Kelvin -> degree
  | Rankine -> 0.
  | Delisle -> 0.
  | Newton -> 0.
  | Reamur -> 0.
  | Romer -> 0.
;;

let fromKelvin degree = function
  | Celsius -> degree -. 273.15
  | Fahrenheit -> (degree *. (9. /. 5.)) -. 459.67
  | Kelvin -> degree
  | Rankine -> 0.
  | Delisle -> 0.
  | Newton -> 0.
  | Reamur -> 0.
  | Romer -> 0. ;;

let convert scale degree =
  let kelvin = toKelvin degree scale in
  
  List.filter (fun (s, _) -> s <> Kelvin) (
    List.map (fun scale ->
    (scale, fromKelvin kelvin scale)
  ) [Celsius; Fahrenheit; Kelvin; Rankine; Delisle; Newton; Reamur; Romer] ) ;;