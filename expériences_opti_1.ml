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
  if (Random.int 10) =0 then ((troublion n); (n,n,Entier(n+1),Entier(n))::(!resultat))
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

let bellman_ford_opti_1 tab s=
  let n=Array.length tab in
  let dist=Array.make n Inf in
  dist.(s)<-Entier(0); 
  let modif=Array.make n true in 
  let stop=ref true in
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
        stop:=false;
        maj_2 q i
    |(a,b)::q->maj_2 q i
  in 
  for k=0 to (n-1) do 
    for i=0 to (n-1) do
      if modif.(i) || i=0 then maj tab.(i) i 
    done;
  done;
  let i=ref 0 in
  while !i<=(n-1) && !stop do
    maj_2 tab.(!i) !i;
    i:= !i+1;
  done;
  !stop,dist;;
  
let d=[(5, 5, Entier 5, Entier 5); (4, 3, Entier 1, Entier 5);
 (2, 3, Entier (-3), Entier 7); (2, 1, Entier (-22), Entier 35);
 (0, 1, Entier 7, Entier 60); (0, 3, Entier (-14), Entier 31)];;

bellman_ford_opti_1 (tableau_opti_1 d) 0;;

(*application et mesures*)


let tab nb sommets dist=
	let t=Array.make nb [||] in
	for i=0 to nb-1 do 
		t.(i)<-tableau_opti_1 (gene_aleat sommets dist)
	done;
	t;;

let rec list nb sommets dist= match nb with
	|0->[]
	|nb-> (tableau (gene_aleat sommets dist))::(list (nb-1) sommets dist)
;;
let mesure t=
	let t1=Sys.time() in
	for i=0 to (Array.length t)-1 do
		ignore (bellman_ford_opti_1 t.(i) 0)
	done;
	Sys.time()-.t1;;

let duree_totale nb_test nb_sommets distance_carac=
	mesure (tab nb_test nb_sommets distance_carac);;
tab 1000 90 100;;
duree_totale 10000 5 100;;
duree_totale 10000 10 100;;
duree_totale 10000 15 100;;
duree_totale 10000 20 100;;
duree_totale 10000 25 100;;
duree_totale 10000 30 100;;
duree_totale 10000 35 100;;
duree_totale 10000 40 100;;
duree_totale 10000 45 100;;
duree_totale 10000 50 100;;
duree_totale 10000 55 100;;
duree_totale 10000 60 100;;
duree_totale 10000 65 100;;
duree_totale 10000 70 100;;
duree_totale 10000 75 100;;
duree_totale 10000 80 100;;
duree_totale 10000 85 100;;
duree_totale 10000 90 100;;
duree_totale 10000 95 100;;
duree_totale 10000 100 100;;
duree_totale 10000 110 100;;
duree_totale 10000 120 100;;
duree_totale 10000 130 100;;
duree_totale 10000 140 100;;
duree_totale 10000 150 100;;
duree_totale 10000 160 100;;
duree_totale 10000 170 100;;
duree_totale 10000 180 100;;
duree_totale 10000 190 100;;
duree_totale 10000 200 100;;


