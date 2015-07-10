let grafo = [(1,[2;3;5]); (5,[2;3;4;1;7]) ;(7,[5;2;3;4]); (2,[4;1;5;3;7]); (3,[2;4;1;5;7]); (4,[2;3;5;7]) ];;

let rec listaNodiGrafo grafo=
  match grafo with 
      []->[]
    | (x,adj)::rest-> x::(listaNodiGrafo rest);;

listaNodiGrafo grafo;;

let rec grafoAdjToGrafoAdjCricca grafo=
  match grafo with
      []->[]
    | (x,adj)::rest-> (x,[])::(grafoAdjToGrafoAdjCricca rest);;

grafoAdjToGrafoAdjCricca grafo;;

let rec contiene adjNodo cricca=
  match cricca with
      []->true
    | x::rest->(List.mem x adjNodo && (contiene adjNodo rest));; 

let rec succNodo graph n=
  match graph with
      []->[]
    | (x,s)::rest->
      if(x==n)
      then s
      else (succNodo rest n);;

let rec succNodoConCricca succ cricca=
  match succ with
      []->[] 
    | x::rest-> (x,cricca)::(succNodoConCricca rest cricca);;

succNodoConCricca (succNodo grafo 1) [2;3];;

let cricca graph k=
  let rec aux coda graph =
    match coda with
        []->[]
      | (x,cricca)::rest-> if((contiene (succNodo graph x) cricca) && ((List.length (x::cricca))==k)) then cricca@[x] 
      else if ((contiene (succNodo graph x) cricca) && ((List.length (x::cricca))<k)) then  (aux (rest@( succNodoConCricca (succNodo graph x) (cricca@[x]))) graph )
      else (aux rest graph)
  in aux (grafoAdjToGrafoAdjCricca graph) graph;;

cricca grafo 5;;

