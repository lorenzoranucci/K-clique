let rec queueForCliqueFromAdjGraph graph k=
  match graph with
      []->[]
    | (x,adj)::rest-> 
        if(List.length adj >= k-1) 
        then(x,[])::(queueForCliqueFromAdjGraph rest k)
        else queueForCliqueFromAdjGraph rest k;;


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
    | x::rest-> if(List.mem x clique) then (succNodeClique rest clique)
        else (x,clique)::(succNodeClique rest clique);;



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
  in aux (queueForCliqueFromAdjGraph graph k) graph;;


let graph = [(1,[2]); (2,[1]) ];;

clique graph 2;;

