(* === Projet Algorithmique et Programmation 3ème année === *)
(* ====== Auteurs : Fréjoux Gaëtan && Niord Mathieu ======= *)

(*use*)
#use "abr.ml";;


(*** === PREMIÈRE PARTIE : Expérimentations/Tests sur les ABR === ***)


(***QUESTION 1***)

(*Test : création d'un arbre avec des valeurs aléatoires*)
let tree1_1 : int bs_tree = bst_rnd_create(10, 100);;
show_int_bst(tree1_1);;
 

(***QUESTION 2***)

(*Plusieurs expérimentations sur le déséquilibre
 moyen d'un abr avec des valeurs aléatoires*)

desequilibre(1, 10, 100000);;

desequilibre(10, 10, 100000);;

desequilibre(100, 100, 100000);;

desequilibre(1000, 1000, 100000);;

desequilibre(10000, 1000, 100000);;

(*Nous avons observé au cours de nos tests que les résultats tendent vers 0.*)
(*ce qui semble cohérent*)

(***QUESTION 3***)

(*Longueurs aléatoires*)
let test  = bst_list_create(2, 5, 5,1000);;
show_int_btree(test);;

list_desequilibre(2, 100, 100, 1000,10);;

(*Longueurs fixe*)
let test  = bst_list_create(0, 5, 5,1000);;
show_int_bst(test);;

list_desequilibre(0, 100, 100, 1000,10);;

(*Longueurs croissante*)
let test  = bst_list_create(1, 5, 5,1000);;
show_int_bst(test);;


list_desequilibre(1, 100, 100, 1000,10);;

(*Longueurs décroissante*)
let test  = bst_list_create(-1, 5, 5,1000);;
show_int_bst(test);;

list_desequilibre(-1, 100, 100, 1000,10);;


(*Tests approfondis*)

list_desequilibre(-1, 100, 100, 1000000, 100);;
list_desequilibre(0, 100, 100, 1000000, 100);;
list_desequilibre(1, 100, 100, 1000000, 100);;
list_desequilibre(2, 100, 100, 1000000, 100);;






(***QUESTION 4***)

(*Rapport d'expérience*)
(*Voir le pdf*)

(*END OF FILE*)
