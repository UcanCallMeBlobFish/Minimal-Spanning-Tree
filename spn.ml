type graph = (int * float * int) list ;;


let sort x = List.sort (fun (k1, w, v1) (k2, w1, v2) -> compare w w1 )  x  ;;

(*function to create list of nodes with sets *)
let rec sets graph =
  let rec sets graph list =
    match graph with 
      [] -> list 
    | (a,k,b)::t -> if List.mem (a,a) list && List.mem (b,b) list then sets t list 
        else if List.mem (a,a) list = false && List.mem (b,b) list then sets t ((a,a)::list) 
        else if List.mem (a,a) list && List.mem (b,b) list = false then sets t ((b,b)::list) 
        else sets t ((a,a)::((b,b)::list)) 
  in sets graph [];; 

(*since we have nodes right side and sets left side find function finds n element and gives its set*)
let rec find n set  = 
  match set with 
    (x,y)::t -> if n = y  then x else find n t 
;;
(*union function find nodes in list and change their sets and make it same*)
let rec union node set = 
  let sett = set in 
  let rec concate node set stack =
    match node, set with 
      (n,w,n1), [] -> stack |
      (n,w,n1), (a,b)::t -> if a = (find n sett) then concate node t (((find n1 sett),b)::stack) else 
          concate node t ((a,b)::stack) 
  in concate node set [];;

(*this is function which checks before we add node if it make cycle . it checks sets and if they have different sets node will be added*)
let iscyclic x list  = 
  match x with 
    (n,w,n1)-> if (find n list) = (find n1 list) then true else false ;;




let  mst graph = 
  let rec mst' graph outputgraph sets = if List.length outputgraph = ((List.length sets) -1) then outputgraph else
      match graph with 
        [] -> outputgraph|
        (n,w,n1)::t -> if (iscyclic (n,w,n1) sets) then mst' t outputgraph sets else mst' t ((n,w,n1)::outputgraph)(union (n,w,n1) sets)
  in
  mst' (sort graph) [] (sets graph);;
  

