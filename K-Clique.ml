let rec queueForCliqueFromAdjGraph graph=
  match graph with
      []->[]
    | (x,adj)::rest-> (x,[])::(queueForCliqueFromAdjGraph rest);;


let rec succNode graph n=
  match graph with
      []->[]
    | (x,s)::rest->
        if(x==n)
        then s
        else (succNode rest n);;

let rec succNodeClique succ clique=
  match succ with
      []->[] 
    | x::rest-> (x,clique)::(succNodeClique rest clique);;



let rec contains adjNode clique=
  match clique with
      []->true
    | x::rest->(List.mem x adjNode && (contains adjNode rest));; 

let clique graph k=
  let rec aux queue graph =
    match queue with
        []->[]
      | (x,clique)::rest-> 
          if((contains (succNode graph x) clique) && ((List.length (x::clique))==k)) then clique@[x] 
          else if ((contains (succNode graph x) clique) && ((List.length (x::clique))<k)) then  (aux (rest@( succNodeClique (succNode graph x) (clique@[x]))) graph )
          else (aux rest graph)
  in aux (queueForCliqueFromAdjGraph graph) graph;;


let graph = [(1,[2;3;5]); (5,[2;3;4;1;7]) ;(7,[5;2;3;4]); (2,[4;1;5;3;7]); (3,[2;4;1;5;7]); (4,[2;3;5;7]) ];;

clique graph 5;;

