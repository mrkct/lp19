open Comparable
open IntervalI


module Interval (Endpoint : Comparable) : IntervalI with type endpoint := Endpoint.t = struct
  type endpoint = Endpoint.t
  type interval = Empty | Interval of (endpoint * endpoint)

  exception WrongInterval

  let create e1 e2 = 
    match Endpoint.compare e1 e2 with
    | 0   -> Empty
    | 1   -> Interval (e1, e2)
    | x   -> raise WrongInterval
  
  let is_empty = function
    | Empty -> true
    | Interval (a, b) -> false
  
  let contains i x =
    match i with
    | Empty -> false
    | Interval (a, b) -> 
      Endpoint.compare a x = 1 && Endpoint.compare x b = 1
  
  let intersect i1 i2 =
    match (i1, i2) with
    | (Empty, _) | (_, Empty) -> Empty
    | (Interval (a1, b1), Interval(a2, b2)) ->
        let left  = if Endpoint.compare a1 a2 >= 0 then a2 else a1 in
        let right = if Endpoint.compare b1 b2 >= 0 then b1 else b2 in
        if left >= right then Empty else Interval (left, right)
  
  let tostring = function
    | Empty -> "[]"
    | Interval (a, b) -> 
        "[" ^ (Endpoint.tostring a) ^ ", " ^ (Endpoint.tostring b) ^ "]"
  
  
end

module IntInterval = Interval(
  struct
    type t = int
    let compare a b = 
      match (a, b) with
      | _ when a > b -> -1
      | _ when a = b -> 0
      | _            -> 1 
    let tostring = string_of_int
  end 
)

module StringInterval = Interval(
  struct
    type t = string
    let compare a b = 
      match (a, b) with
      | _ when a > b -> -1
      | _ when a = b -> 0
      | _            -> 1 
    
    let tostring s = s
  end
)
