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
(*2.1.2*)
(*2.1.3*)
(*2.1.4*)

(*2.2 Expérimentations avec les abres AVL*)

(*2.2.1*)
(*2.2.2*)
