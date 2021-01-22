(*Projet d'algorithmique et Programmation*)
(*Auteur : Fréjoux Gaëtan && Niord Mathieu*)

(*directory*)
#directory "Modules";;

(*open*)
open Btree;;
open Bst;;

(*use*)
#load "btree.cmo";;
#load "bst.cmo";;
#use "fonctions.ml"

(*1 Arbres binaires de recherche*)

(*1.1*)

(*Fonctions*)

(*Applications*)
(*Fonction créant un arbre binaire de recherche avec des valeurs aléatoires*)
let bst_rnd_create(sizeTree,nbMax : int*int) : int bs_tree =
  Random.self_init ();
  let answer : int bs_tree ref = ref (empty()) in
  
  for i = 0 to sizeTree do
    answer :=  bst_linsert(!answer , Random.int nbMax)
  done;
  !answer
;;






(*1.2*)

(*Fonctions*)

(*Fonction qui calcule le désequilibre d'un arbre de recherche*)
let desequilibre(nbTree,sizeTree,nbMax : int*int*int) : int =
  let somme : int ref = ref 0 and
      t : int bs_tree ref = ref (empty()) in
  for i = 0 to nbTree do
    t := bst_rnd_create(sizeTree,nbMax);
    somme := !somme + (height(lson(!t)) - height(rson(!t)));
  done;
  (!somme)/nbTree
;;

(*Applications*)

desequilibre(1,10,1000);;
(*Resultat obtenu : 8*)

desequilibre(1,100,1000);;
(*Resultat obtenu : 14*)

desequilibre(1,1000,1000);;
(*Resultat obtenu : 2*)

desequilibre(10,1000,1000);;
(*Resultat obtenu : 5*)

desequilibre(100,1000,1000);;
(*Resultat obtenu : 5*)

desequilibre(1000,1000,1000);;
(*Resultat obtenu : 5*)

desequilibre(10000,1000,1000);;
(*Resultat obtenu : 5*)

desequilibre(10000,10000,1000);;
(*Resultat obtenu : 5*)



(*Nous observons lors de nos tests que les résultats tendent vers 5.*)

(*1.3*)

let bst_list_create(mode,nbList,nbElement) : int bs_tree =
  Random.self_init ();
  let answer : int bs_tree ref = ref (empty()) and
      listLen : int ref = ref nbList and
      random : int ref  = ref (Random.int 10000 + nbList*nbElement) and
      elemLen : int ref = ref nbElement in
  
  if(mode = -1) (*longueur décroissante*)
  then ( 
    while(!listLen>0) do
      (
        for i = 0 to !elemLen do
          answer :=  bst_linsert(!answer , !random);
          random := !random + 1;
        done;
        random := !random + Random.int 100;
        elemLen := !elemLen-1;
        listLen:= !listLen -1
      )
    done;
    !answer
  )
  else if(mode = 0) (*longueur fixe*)
  then (
    while(!listLen>0) do
      (
        for i = 0 to !elemLen do
          answer :=  bst_linsert(!answer , !random);
          random := !random + 1;
        done;
        random := !random + Random.int 100;
        listLen:= !listLen -1;
      )
    done;
    !answer
  )
  else if(mode = 1) (*longueur croissante*)
  then (
    while(!listLen>0) do
      (
        for i = 0 to !elemLen do
          answer :=  bst_linsert(!answer , !random);
          random := !random + 1;
        done;
        random := !random + Random.int 100;
        elemLen := !elemLen+1;
        listLen:= !listLen -1;
      )
    done;
    !answer
  )
  else if(mode = 2) (*longueur aléatoire*)
  then (
    while(!listLen>0) do
      (
        elemLen := Random.int nbElement;
        
        for i = 0 to !elemLen do
          answer :=  bst_linsert(!answer , !random);
          random := !random + 1 ;
        done;
        
        random := !random + Random.int 100;
        listLen:= !listLen -1;
      )
    done;
    !answer
  )
  else !answer
;;


(*Fonctions*)

(*Applications*)

(*Longueurs aléatoires*)
(*Longueurs fixe*)
(*Longueurs croissante*)
(*Longueurs décroissante*)




(*1.4*)

(*Fonctions*)

(*Applications*)













(*2 Arbres AVL*)

(*2.1 Implantation d'un module Avl*)


(*2.1.1*)

(*Fonctions*)

let rg(tree : 'a bs_tree) : 'a bs_tree =
  if( not(isEmpty(tree)) && not(isEmpty(rson(tree))))
  then rooting(
           root(rson(tree)),
           rooting(root(tree),lson(tree),lson(rson(tree))),
           rson(rson(tree))
         )
  else failwith "Error, cannot rotate this tree"

;;

let rd(tree : 'a bs_tree) : 'a bs_tree =
  if( not(isEmpty(tree)) && not(isEmpty(lson(tree))))
  then rooting(
           root(lson(tree)),
           lson(lson(tree)),
           rooting(root(tree),rson(lson(tree)),rson((tree)))
         )
  else failwith "Error, cannot rotate this tree"

;;

let rgd(tree : 'a bs_tree) : 'a bs_tree =
  if( not(isEmpty(tree))
      && not(isEmpty(lson(tree)))
      && (not(isEmpty(rson(tree)))
          && not(isEmpty(rson(lson(tree))))))
  then rooting(
           root(rson(lson(tree))),
           rooting(
               root(lson(tree)),
               lson(lson(tree)),
               lson(rson(lson(tree)))
             ),
           rooting(
               root(tree),
               rson(rson(lson(tree))),
               rson(tree)
             )
         )
  else failwith "Error, cannot rotate this tree"
;;



let rdg(tree : 'a bs_tree) : 'a bs_tree =
  if( not(isEmpty(tree))
      && not(isEmpty(rson(tree)))
      && (not(isEmpty(lson(tree)))
          && not(isEmpty(lson(rson(tree))))))
  then  rooting(
            root(lson(rson(tree))),
            rooting(
                root(tree),
                lson(tree),
                lson(lson(rson(tree)))
              ),
            rooting(
                root(rson(tree)),
                rson(lson(rson(tree))),
                rson(rson(tree))
              )
         )
  else failwith "Error, cannot rotate this tree"  
;;
(*Applications*)


(*Arbre de test*)
let t1 : int bs_tree = rooting(5,
                               rooting(3,rooting(2,empty(),empty()),rooting(4,empty(),empty())),
                               rooting(7,rooting(6,empty(),empty()),rooting(9,empty(),empty())))
;;


show_int_bst(t1);;

(*rd*)
let t1_after = rd(t1);;
show_int_bst(t1_after);;

(*rg*)
let t1_after = rg(t1);;
show_int_bst(t1_after);;

(*rgd*)
let t1_after = rgd(t1);;
show_int_bst(t1_after);;

(*rdg*)
let t1_after = rdg(t1);;
show_int_bst(t1_after);;



(*2.1.2*)

(*type d'arbre stockant le déséquilibre*)
type 'a avl_btree = (int * 'a) t_btree;;

let show_int_avl (tree :int avl_btree) = show((fun (e,r:int*int) ->
                                             String.concat "," [(string_of_int e);(string_of_int r)]),
                                              tree);; 
let avlt : int avl_btree = rooting((-2,5),empty(),rooting((-1,6),empty(),rooting((0,7),empty(),empty())));;
show_int_avl(avlt);;
let getEquilibre(tree :'a avl_btree) : int =
  if(isEmpty(tree))
  then failwith "Tree is empty"
  else
    let (e,r) : (int*'a) = root(tree) in
    e
;;
 
let reequilibrer(tree : 'a avl_btree) : 'a avl_btree =
  let e :int =getEquilibre(tree) in
  if (e=2 && getEquilibre(lson(tree))=1)
  then rd(tree)
  else if (e=2 && getEquilibre(lson(tree))= -1 )
  then rgd(tree)
  else if (e= -2 && getEquilibre(rson(tree))= -1)
  then rg(tree)
  else if (e= -2 && getEquilibre(rson(tree))= 1)
  then rdg(tree)
  else tree
;;

let t12 = reequilibrer(avlt);;
show_int_avl(t12);;

  
(*2.1.3*)

let getRoot(tree : 'a avl_btree) : 'a =
  if(isEmpty(tree))
  then failwith "Tree is empty"
  else
    let (e,r) : (int*'a) = root(tree) in
    r
;;

let rec ajt_avl(elem,tree : ' a * 'a avl_btree) : 'a avl_btree =
  if(isEmpty(tree))
  then rooting((0,elem),empty(),empty())
  else let (r,g,d) = (root(tree)),lson(tree),rson(tree) in 
       if (elem < getRoot(tree))
       then reequilibrer(rooting(r, ajt_avl(elem, g), d))
       else
         if (elem > getRoot(tree))
         then reequilibrer(rooting(r, g, ajt_avl(elem, d)))
         else rooting(r, g, d)
;;

let rec max_avl(tree : 'a avl_btree) : 'a =
  if(isEmpty(tree))
  then failwith "Tree is empty"
  else if(isEmpty(rson(tree)))
  then getRoot(tree)
  else max_avl(rson(tree))
;;

let rec dmax_avl(tree : 'a avl_btree) : 'a avl_btree =
  if(isEmpty(tree))
  then failwith "Tree is empty"
  else if(isEmpty(rson(tree)))
  then lson(tree)
  else reequilibrer(rooting(root(tree),lson(tree),dmax_avl(rson(tree))))
;;

let isLeaf(tree : 'a avl_btree) : bool =
  if(isEmpty(tree))
  then failwith "Tree is empty"
  else (isEmpty(lson(tree)) && isEmpty(rson(tree)))
;;

let rec suppr_avl(elem,tree : 'a * 'a avl_btree) : 'a avl_btree =
  if(isEmpty(tree))
  then empty()
  else let (v,r,g,d : 'a*(int *'a)*'a avl_btree*'a avl_btree) = (getRoot(tree),root(tree),lson(tree),rson(tree)) in
       if(elem < v )
       then reequilibrer(rooting(r,suppr_avl(elem, g),d))
       else
         if(elem > v)
         then reequilibrer(rooting(r, g, suppr_avl(elem, d)))
         else if(elem = v && not(isEmpty(d)))
         then if(not(isEmpty(g)))
              then reequilibrer(rooting(max_avl(g), dmax_avl(g), d))
              else d
         else g
;;

(*2.1.4*)

(*2.2 Expérimentations avec les abres AVL*)

(*2.2.1*)
(*2.2.2*)
