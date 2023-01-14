type int_inf= Inf|Entier of int;;

(*fonctions opératoires*)

let minore x y =match x,y with
  |Inf,Inf->true
  |Inf,_->false
  |_,Inf->true
  |Entier(a),Entier(b)->a<(b);;

let add x y=match x,y with
  |Inf,_->Inf
  |_,Inf->Inf
  |Entier(a),Entier(b)->Entier(a+b);;

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

(*generateur*)



let gene_aleat n dist= 
  let evenements=Array.make n 0 in
  for i=1 to (n-1) do
    evenements.(i)<-Random.int (2*dist)
  done;
  let liens=Array.make_matrix n n false in
  for i=0 to (n-1) do
    liens.(i).(i)<-true
  done; 
  let resultat= ref [] in
  let connection=Array.make n false in
  connection.(0)<-true;
  let connectes=ref 1 in
  while !connectes<n do
    let i=Random.int n in
    let j=Random.int n in
    if liens.(i).(j)=false then(
      liens.(i).(j)<-true;
      liens.(j).(i)<-true;
      let maj (i,j) =match connection.(i),connection.(j) with
        |(true,true)->()
        |(false,true)->
            connection.(i)<-true;
            connectes:= !connectes+1;
        |(true,false)->
            connection.(j)<-true;
            connectes:= !connectes+1; 
        |(_,_)->()
      in
      if evenements.(i)<evenements.(j) then
        let ecart=evenements.(j)-evenements.(i) in
        resultat:= (i,j,Entier((Random.int (2*ecart+1))-ecart-1),Entier(ecart+Random.int(ecart+1)))::(!resultat) ;
        maj (i,j);
      else
        let ecart=evenements.(i)-evenements.(j) in
        resultat:= (j,i,Entier((Random.int (2*ecart+1))-ecart-1),Entier(ecart+Random.int(ecart+1)))::(!resultat) ;
        maj (j,i);
    ) 
  done; 
  let troublion n=
    let change=Random.int((List.length !resultat) -2) in
    let rec bouge k l=match k,l with
      |(0,(a,b,c,d)::q1)-> let (a1,b1,c1,d1)::q=q1 in (a1,b1,add c d,add d (oppose c))::(a,b,add d1 c1,add d1 (oppose c1))::q
      |(k,t::q)-> t::(bouge (k-1) q)
    in 
    resultat:=(bouge change !resultat);
  in
  if (Random.int 2) =0 then ((troublion n); (n,n,Entier(n+1),Entier(n))::(!resultat))
  else (n,n,Entier(n),Entier(n))::(!resultat)
;;

let l=[(6,6,Entier(6),Entier(6));(0,3,Entier(0),Entier(45));(0,5,Entier(0),Entier(90));(0,1,Entier(2),Entier(15));(0,2,Entier(0),Entier(15));(1,2,Entier(3),Entier(7));(2,3,Entier(25),Entier(30));(3,4,Entier(25),Entier(35));(4,5,Entier(15),Inf)] ;;

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
	let stop=ref true in
	let i=ref 1 in
  while !i<(Array.length l) && !stop do
    let (a,b,c,d)=l.(!i) in
    i:=!i+1;
    if dist.(b)<>Inf && minore (add dist.(a) (d)) dist.(b) then
      stop:=false;
    if minore (add dist.(b) (oppose c)) dist.(a) then
      stop:=false;
  done;
  !stop,dist
;;

(*application et mesures*)


let tab nb sommets dist=
	let t=Array.make nb [||] in
	for i=0 to nb-1 do 
		t.(i)<-tableau (gene_aleat sommets dist)
	done;
	t;;
let rec list nb sommets dist= match nb with
	|0->[]
	|nb-> (tableau (gene_aleat sommets dist))::(list (nb-1) sommets dist)
;;
let mesure t=
	let t1=Sys.time() in
	for i=0 to (Array.length t)-1 do
		ignore (bellman_ford t.(i) 0)
	done;
	Sys.time()-.t1;;

let duree_totale nb_test nb_sommets distance_carac=
	mesure (tab nb_test nb_sommets distance_carac);;
tab 1000 90 100;;
duree_totale 10000 100 100;;
