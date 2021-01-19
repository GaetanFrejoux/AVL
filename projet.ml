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



(*Fonction auxiliaire*)

(*Fonction permettant de calculer la hauteur d'un arbre binaire*)
let rec height(tree : 'a bs_tree) : int =
  if(isEmpty(tree) || (isEmpty(lson(tree))&&isEmpty(rson(tree))))
  then 0
  else 1+max (height(lson(tree))) (height(rson(tree)))
;;












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
    somme := !somme + abs (height(lson(!t)) - height(rson(!t)));
  done;
  (!somme)/nbTree
;;

(*Applications*)

(*desequilibre(1,10,1000);;*)
(*Resultat obtenu : 8*)

(*desequilibre(1,100,1000);;*)
(*Resultat obtenu : 14*)

(*desequilibre(1,1000,1000);;*)
(*Resultat obtenu : 2*)

(*desequilibre(10,1000,1000);;*)
(*Resultat obtenu : 5*)

(*desequilibre(100,1000,1000);;*)
(*Resultat obtenu : 5*)

(*desequilibre(1000,1000,1000);;*)
(*Resultat obtenu : 5*)

(*desequilibre(10000,1000,1000);;*)
(*Resultat obtenu : 5*)

(*desequilibre(10000,10000,1000);;*)
(*Resultat obtenu : 5*)



(*Nous observons lors de nos tests que les résultats tendent vers 5.*)

(*1.3*)



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
           lson(lson(tree))),
           rooting(root(tree),rson(tree),rson(lson(tree))
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
                lson(lson(rson(tree))),
                lson(tree)
              ),
            rooting(
                root(rson(tree)),
                rson(rson(tree)),
                rson(lson(rson(tree)))
              )
         )
  else failwith "Error, cannot rotate this tree"
    
;;
(*2.1.2*)

let reequilibrer(tree : 'a bs_tree) : 'a bs_tree =
  tree
;;

  
(*2.1.3*)
(*2.1.4*)

(*2.2 Expérimentations avec les abres AVL*)

(*2.2.1*)
(*2.2.2*)
