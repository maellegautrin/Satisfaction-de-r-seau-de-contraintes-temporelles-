type int_inf= Inf|Entier of int;;


let add x y=match x,y with
  |Inf,_->Inf
  |_,Inf->Inf
  |Entier(a),Entier(b)->Entier(a+b);;

let oppose c=
  let Entier(a)=c in
  Entier(-a);;



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
gene_aleat 5 5;;