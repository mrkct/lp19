open NaturalI


module Natural : NaturalI = 
  struct
    type natural = Zero | Succ of natural
    exception NegativeNumber
    exception DivisionByZero

    let rec eval = function
      | Zero -> 0
      | Succ (x) -> 1 + (eval x)
    
    let rec convert = function
      | 0 -> Zero
      | x -> 
        if x > 0 then 
          Succ(convert (x-1)) 
        else 
          raise NegativeNumber
    
    let rec ( + ) a b =
      match (a, b) with
      | (Zero, b) -> b
      | (a, Zero) -> a
      | (a, Succ(b)) -> Succ(a) + b
    
    let rec ( - ) a b =
      match (a, b) with
      | (Zero, Succ(b)) -> raise NegativeNumber
      | (a, Zero) -> a
      | (Succ(a), Succ(b)) -> a - b
    
    let rec ( * ) a b =
      match (a, b) with
      | (Zero, _) | (_, Zero) -> Zero
      | (a, Succ(Zero)) -> a
      | (Succ(Zero), b) -> b
      | (a, Succ(b)) -> (a * b) + a
    
    let rec compare a b =
      match (a, b) with
      | (Zero, Succ(b)) -> 1
      | (Succ(a), Zero) -> -1
      | (Zero, Zero)    -> 0
      | (Succ(a), Succ(b)) -> compare a b
    
    let rec ( / ) a b =
      match (a, b) with
      | (_, Zero) -> raise DivisionByZero
      | (Zero, _) -> Zero
      | (a, b) -> 
        if compare a b = 1 then Zero else Succ(((a - b) / b))
  end

module N = (Natural: NaturalI)