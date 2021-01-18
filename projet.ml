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

(*1 Arbres binaires de recherche*)

let rec height(tree : 'a bs_tree) : int =
  if(isEmpty(tree) || (isEmpty(lson(tree))&&isEmpty(rson(tree))))
  then 0
  else 1+max (height(lson(tree))) (height(rson(tree)))
;;


let bst_rnd_create(n : int) : int bs_tree =
  let list : int list ref= ref [] in
  for i = 0 to n do
    list := (Random.int 10000000)::(!list);
  done;
  bst_lbuild(!list)
;;


let equilibre(n : int) : int =
  let somme : int ref = ref 0 and
      t : int bs_tree ref = ref (empty()) in
  for i = 0 to n do
    t := bst_rnd_create(10000);
    somme := !somme + abs (height(lson(!t)) - height(rson(!t)));
  done;
  (!somme)/n
;;



(*1.1*)

(*1.2*)

(*1.3*)

(*1.4*)




(*2 Arbres AVL*)

(*2.1 Implantation d'un module Avl*)

(*2.1.1*)
(*2.1.2*)
(*2.1.3*)
(*2.1.4*)

(*2.2 Expérimentations avec les abres AVL*)

(*2.2.1*)
(*2.2.2*)
