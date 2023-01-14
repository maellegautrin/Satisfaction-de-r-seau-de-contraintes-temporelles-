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



let tableau l= 
  let n=List.length l in
  let u=Array.make n (0,0,Entier(0),Entier(0)) in
  let rec remplissage i l= match l with
    |[]->u
    |t::q-> u.(i)<-t; remplissage (i+1) q
  in 
  remplissage 0 l;;
tableau l;;



(*fonction calcul*)



let bellman_ford l s=
  let (a,b,c,d)= l.(0) in
  let n=a in
  let dist=Array.make n Inf in
  dist.(s)<-Entier(0);
  for k=0 to n-1 do
    for i=1 to (Array.length l)-1 do
      let (a,b,c,d)=l.(i) in
      if minore (add dist.(a) d) dist.(b) then
        (dist.(b)<-add dist.(a) d)
    done;
    for i=1 to (Array.length l)-1 do
      let (a,b,c,d)=l.(i) in
      if minore (add dist.(b) (oppose c)) dist.(a) then
        dist.(a)<-add dist.(b) (oppose c);
    done;
  done;

  for i=1 to (Array.length l)-1 do
    let (a,b,c,d)=l.(i) in
    if dist.(b)<>Inf && minore (add dist.(a) (d)) dist.(b) then
      failwith "incohérent";
    if minore (add dist.(b) (oppose c)) dist.(a) then
      failwith "incohérent";
  done;
  dist
;;



(*application*)


bellman_ford (tableau l) 0;;