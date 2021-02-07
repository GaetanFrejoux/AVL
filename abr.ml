(* === Projet Algorithmique et Programmation 3ème année === *)
(* ====== Auteurs : Fréjoux Gaëtan && Niord Mathieu ======= *)

(*directory*)
(*suivant la version ocaml, choisir le bon répertoire : *)
(*#directory "Modules/4.02.1+ocp1";;*)
(*
#directory "Modules/4.02.3";;
#directory "Modules/4.05.0";;
#directory "Modules/4.08.1";;
#directory "Modules/4.10.0";;
#directory "Modules/4.11.1";;
 *)
#directory "Modules/4.08.1";;
(*open*)
open Btree;;

(*use et load*)
#load "btree.cmo";;
#use "bst.ml";;


(*** === PREMIÈRE PARTIE : Arbres Binaires de Recherche (ABR) === ***)


(***QUESTION 1***)

(*Initialisation du générateur de valeur aléatoire*)
Random.self_init ();;
(*FONCTIONS*)

(*Fonction qui créé un arbre binaire de recherche avec des valeurs aléatoires*)
let rec bst_rnd_create(sizeTree, nbMax : int * int) : int bs_tree =
  if(sizeTree = 0)
  then empty()
  else let t = bst_rnd_create((sizeTree-1),nbMax) in
       bst_linsert(t,Random.int nbMax)
;;

(***QUESTION 2***)

(*FONCTIONS*)

(*Fonction qui renvoie la hauteur d'un arbre binaire*)
let rec height(tree : 'a t_btree) : int =
  if(isEmpty(tree) || (isEmpty(lson(tree)) && isEmpty(rson(tree))))
  then 0
  else 1 + max (height(lson(tree))) (height(rson(tree)))
;;


(*Fonction qui calcule le déséquilibre d'un arbre binaire de recherche*)
let desequilibre(nbTree, sizeTree, nbMax : int * int * int) : float =
  let somme : float ref = ref 0. and
      t : int bs_tree ref = ref (empty()) in
  for i = 0 to nbTree do
    t := bst_rnd_create(sizeTree, nbMax);
    somme := !somme +. float_of_int(height(lson(!t)) - height(rson(!t)));
  done;
  (!somme)/.float_of_int(nbTree)
;;


(***QUESTION 3***)

(*FONCTIONS*)

(*Fonction qui construit un abr selon un mode choisi
  mode -1 : avec liste décroissante
  mode 0  : avec liste de meme longueur
  mode 1  : avec liste croissante
  mode 2  : avec liste de taille aléatoire
 *)
let bst_list_create(mode, nbList, nbElement, nbMax) : int bs_tree =
  Random.self_init ();
  let answer : int bs_tree ref = ref (empty()) and
      listLen : int ref = ref nbList and
      random : int ref  = ref (Random.int nbMax) and
      elemLen : int ref = ref nbElement in
  if(mode < 3 && mode > -2)
  then (
    while(!listLen > 0) do
      (
        (*Dans le cas où le mode choisi est celui de taille aléatoire,
            la taille de la liste est défini juste ici :*)
        if(mode = 2) 
        then elemLen := Random.int nbElement;  
        for i = 0 to !elemLen do
          answer :=  bst_linsert(!answer, !random);
          random := !random + Random.int 100;
        done;
        random :=  Random.int nbMax;
        elemLen := !elemLen + mode;
        listLen:= !listLen - 1;
      )
    done;
    !answer
  )
  else rooting(0,empty(),empty())
;;

let list_desequilibre(mode, nbList, nbElement,nbMax ,nbTree) : int =
  let somme : int ref = ref 0 and
      t : int bs_tree ref = ref (empty()) in
  for i = 0 to nbTree do
    t := bst_list_create(mode, nbList, nbElement,nbMax);
    somme := !somme + (height(lson(!t)) - height(rson(!t)));
  done;
  (!somme)/nbTree
;;


(*END OF FILE*)
