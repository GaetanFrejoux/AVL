(* === Projet Algorithmique et Programmation 3�me ann�e === *)
(* ====== Auteurs : Fr�joux Ga�tan && Niord Mathieu ======= *)

(*use*)
#use "abr.ml";;


(*** === PREMI�RE PARTIE : Exp�rimentations/Tests sur les ABR === ***)


(***QUESTION 1***)

(*Test : cr�ation d'un arbre avec des valeurs al�atoires*)
let tree1_1 : int bs_tree = bst_rnd_create(10, 100);;
show_int_bst(tree1_1);;
 

(***QUESTION 2***)

(*Plusieurs exp�rimentations sur le d�s�quilibre
 moyen d'un abr avec des valeurs al�atoires*)

desequilibre(1, 10, 100000);;

desequilibre(10, 10, 100000);;

desequilibre(100, 100, 100000);;

desequilibre(1000, 1000, 100000);;

desequilibre(10000, 1000, 100000);;

(*Nous avons observ� au cours de nos tests que les r�sultats tendent vers 0.*)
(*ce qui semble coh�rent*)

(***QUESTION 3***)

(*Longueurs al�atoires*)
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

(*Longueurs d�croissante*)
let test  = bst_list_create(-1, 5, 5,1000);;
show_int_bst(test);;

list_desequilibre(-1, 100, 100, 1000,10);;


(*Tests approfondis*)

list_desequilibre(-1, 100, 100, 1000000, 100);;
list_desequilibre(0, 100, 100, 1000000, 100);;
list_desequilibre(1, 100, 100, 1000000, 100);;
list_desequilibre(2, 100, 100, 1000000, 100);;






(***QUESTION 4***)

(*Rapport d'exp�rience*)
(*Voir le pdf*)

(*END OF FILE*)
