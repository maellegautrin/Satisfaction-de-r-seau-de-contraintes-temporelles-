type int_inf= Inf|E of int;;

let l=[(6,6,E(6),E(6));(0,3,E(0),E(40));(0,5,E(0),E(60));(0,1,E(5),E(10));
       (0,2,E(0),E(14));(1,2,E(5),E(7));(2,3,E(3),E(8));(3,4,E(10),E(20));
       (4,5,E(5),E(10))] ;;



(*fonctions opératoires*)



let add x y=match x,y with 
  |Inf,_->Inf
  |_,Inf->Inf
  |E(a),E(b)->E(a+b);;

let mini x y = match x,y with
  |Inf,_->y
  |_,Inf->x
  |E(a),E(b)->E(min a b);;

let minore x y =match x,y with
  |Inf,Inf->true
  |Inf,_->false
  |_,Inf->true
  |E(a),E(b)->a<(b);;

let oppose x= match x with
  |Inf->Inf
  |E(a)->E(-a);;

let composition it jt is js graphe=
  (add (graphe.(jt).(it)) (graphe.(js).(is))),
  add (graphe.(it).(jt)) (graphe.(is).(js));;

let intersection reseau (i,j) (a,b)=
  (mini (reseau.(j).(i)) a), (mini reseau.(i).(j) b);;


(*matrice*)


let matrice_adj l=
  let (n,_,_,_)::q=l in 
  let m=Array.make_matrix n n Inf in
  let rec aux l= match l with
    |[]->()
    |(r,t,y,u)::p-> m.(r).(t)<-u;
        m.(t).(r)<-oppose(y);
        aux p;
  in 
  for i=0 to n-1 do
    m.(i).(i)<-E(0);
  done;
  aux q;
  m,q;;



(*fonctions de calcul*)



let rec pc_failure i j t=
  if i>=j then 
    match t.(i).(j),t.(j).(i) with
    |E(a),_ -> minore t.(j).(i) (E(-a))
    |Inf,_ -> false 
  else 
    match t.(i).(j),t.(j).(i) with
    |Inf,_-> false
    |_,Inf-> false
    |E(a),E(b)-> (-b)>a 
;;


let pc graphe= 
  let n= Array.length graphe in
  let t= Array.copy graphe  in
  let b=ref true in
  while !b do
    b:=false;
    for k= 0 to n-1 do
      for i=0 to n-1 do
        for j=0 to n-1 do
          let intetcomp=intersection t (i,j) (composition i k k j t) in
          if (not !b)&&(intetcomp<>(t.(j).(i),t.(i).(j))) then b:=true;
          t.(i).(j)<-snd(intetcomp);
          t.(j).(i)<-(fst(intetcomp));
          if pc_failure i j t then failwith"non coherent"
        done;
      done;
    done;
  done;
  t;;



(*trouver une solution*)



let minimiser_valeur graphe i j=
  if (minore (graphe.(i).(j))( E(0))) then 
    graphe.(j).(i)<- (oppose graphe.(i).(j))	
  else graphe.(i).(j)<- (oppose graphe.(j).(i));
  graphe;;

let rec solution graphe l = match l with 
  |[]-> graphe
  |t::q -> let (it,jt,_,_)=t in
      solution (pc (minimiser_valeur graphe it jt)) q
;; 
 

 
(*solution finale*)



let recherche_solution_final l=
  let m,q=matrice_adj l in
  solution (pc m) q;;

pc( fst( matrice_adj l));;