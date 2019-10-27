open Printf


module SocialNetwork : sig
  type mapkey
  type person
  type network

  val create: unit -> network
  val add: network -> string -> int -> network
  val friend: network -> string -> string -> unit
  val unfriend: network -> string -> string -> unit
  val print: network -> unit
end = struct
  type mapkey = string
  type person = {name: string; age: int; connections: mapkey list}
  type network = {map: (mapkey, person) Hashtbl.t}
  
  let create () = {map=Hashtbl.create 100}

  let add net name age = 
    Hashtbl.add net.map name {name=name; age=age; connections=[]} ; net
  
  let print n = 
    Hashtbl.iter (fun k v -> 
      printf "[%s]{name: %s; age: %d}. Friends: \n" k v.name v.age ;
      List.iter (fun n -> printf "\t%s\n" n) v.connections
    ) n.map
  
  let friend net name1 name2 =
    let p1 = Hashtbl.find net.map name1 in
    let p2 = Hashtbl.find net.map name2 in
    Hashtbl.replace net.map name1 {name=p1.name; age=p1.age; connections=name2 :: p1.connections} ;
    Hashtbl.replace net.map name2 {name=p2.name; age=p2.age; connections=name1 :: p2.connections}
  
  let unfriend net name1 name2 =
    let p1 = Hashtbl.find net.map name1 in
    let p2 = Hashtbl.find net.map name2 in
    let p1_friends = List.filter (fun x -> x <> name2) p1.connections in
    let p2_friends = List.filter (fun x -> x <> name1) p2.connections in
    Hashtbl.replace net.map name1 {name=p1.name; age=p1.age; connections=p1_friends} ;
    Hashtbl.replace net.map name2 {name=p2.name; age=p2.age; connections=p2_friends}
end

let () =
  let n = SocialNetwork.create () in
  SocialNetwork.add n "Marco" 21 ;
  SocialNetwork.add n "Pippo" 28 ;
  SocialNetwork.add n "Maria" 24 ;
  SocialNetwork.friend n "Marco" "Pippo" ;
  SocialNetwork.friend n "Pippo" "Maria" ;
  SocialNetwork.friend n "Marco" "Maria" ;
  SocialNetwork.print n ;
  printf "--------\n" ;
  SocialNetwork.unfriend n "Marco" "Pippo" ;
  SocialNetwork.print n 