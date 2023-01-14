type int_inf= Inf|Entier of int;;


let l=[(6,6,Entier(6),Entier(6));(0,3,Entier(0),Entier(45));(0,5,Entier(0),Entier(90));(0,1,Entier(2),Entier(15));(0,2,Entier(0),Entier(15));(1,2,Entier(3),Entier(7));(2,3,Entier(25),Entier(30));(3,4,Entier(25),Entier(35));(4,5,Entier(15),Inf)] ;;
let g=[(3,3,3,3);(0,1,10,20);(1,2,30,35);(0,2,0,50)];;



(*fonctions opératoires*)



let add x y=match x,y with
  |Inf,_->Inf
  |_,Inf->Inf
  |Entier(a),Entier(b)->Entier(a+b);;

let minore x y =match x,y with
  |Inf,Inf->true
  |Inf,_->false
  |_,Inf->true
  |Entier(a),Entier(b)->a<(b);;

let oppose c=
  let Entier(a)=c in
  Entier(-a);;


(*tableau*) 

let tableau_opti_1 l=
  let (n,_,_,_)::q=l in
  let u=Array.make n [] in
  let rec remplissage q=match q with
    |[]->u
    |(a,b,c,d)::t-> u.(a)<-(b,d)::u.(a); u.(b)<-(a,oppose c)::u.(b) ;remplissage t
  in 
  remplissage q;;
tableau_opti_1 l;;


(*fonction calcul*) 

let bellman_ford_opti_1 l s=
  let (a,b,c,d)::queue= l in
  let n=a in
  let dist=Array.make n Inf in
  dist.(s)<-Entier(0); 
  let tab=tableau_opti_1 l in
  let modif=Array.make n true in 
  let rec maj liste i= match liste with
    |[]->modif.(i)<-false;
    |(a,b)::q when minore (add dist.(i) b) dist.(a) ->
        (dist.(a)<-add dist.(i) b);modif.(a)<-true;
        maj q i
    |(a,b)::q->maj q i
  in 
  let rec maj_2 liste i= match liste with
    |[]->modif.(i)<-false;
    |(a,b)::q when minore (add dist.(i) b) dist.(a)->
        failwith "incohérent";
        maj_2 q i
    |(a,b)::q->maj_2 q i
  in 
  for k=0 to (n-1) do 
    for i=0 to (n-1) do
      if modif.(i) || i=0 then maj tab.(i) i 
    done;
  done;
  for i=0 to (n-1) do
    maj_2 tab.(i) i
  done;
  dist;;
  
      

(*application*)

bellman_ford_opti_1 l 0;;