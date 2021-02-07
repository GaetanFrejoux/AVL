(*Projet d'algorithmique et Programmation*)
(*Auteur : Fréjoux Gaëtan && Niord Mathieu*)

#use "avl.ml";;

(*Ce fichier .ml permet de tester les différentes fonctions pour les avl*)

(*2 Arbres AVL*)

(*2.1 Implantation d'un module Avl*)

(*2.1.1*)

(*rd*)


let rd_tree = ajt_avl(7,ajt_avl(2,ajt_avl(15,ajt_avl(5,ajt_avl(10,empty())))));;
show_int_avl(rd_tree);;
show_int_avl(rd(rd_tree));;

(*rg*)

let rg_tree = ajt_avl(17,ajt_avl(12,ajt_avl(15,ajt_avl(5,ajt_avl(10,empty())))));;
show_int_avl(rg_tree);;
show_int_avl(rg(rg_tree));;

(*rgd*)

let rgd_tree = ajt_avl(12,ajt_avl(17,ajt_avl(15,ajt_avl(5,ajt_avl(35,ajt_avl(30,ajt_avl(10,ajt_avl(20,empty()))))))));;
show_int_avl(rgd_tree);;
show_int_avl(rgd(rgd_tree));;

(*rdg*)

let rdg_tree = ajt_avl(27,ajt_avl(23,ajt_avl(25,ajt_avl(40,ajt_avl(5,ajt_avl(30,ajt_avl(10,ajt_avl(20,empty()))))))));;
show_int_avl(rdg_tree);;
show_int_avl(rdg(rdg_tree));;




(*2.1.2*)
(*Pour tester le reequilibrage, nous utiliserons la fonction d'ajout qui utilise cette fonction*)
show_int_avl(ajt_avl(9,ajt_avl(8,ajt_avl(7,ajt_avl(6,ajt_avl(5,ajt_avl(4,ajt_avl(3,ajt_avl(2,ajt_avl(1,empty()))))))))));;

(*2.1.3*)
let ajt_tree = avl_rnd_create(100,10);;
show_int_avl(ajt_tree);;
show_int_avl(ajt_avl(73,ajt_tree));;

(*2.1.4*)

let seek_tree = ajt_avl(7,ajt_avl(321361,ajt_avl(2189,ajt_avl(1,ajt_avl(200,ajt_avl(10,ajt_avl(50,empty())))))));;
show_int_avl(seek_tree);;
show_int_avl(seek_avl(2189,seek_tree));;


(*2.2 Expérimentations avec les abres AVL*)

(*2.2.1*)

(*Ajout*)

let averageTimePerPowerAdd (nbMax,power,nbTime : int *float*int) : float  =
  let tree : int avl ref = ref (empty()) in
  let time : float ref = ref 0. in
  let somme : float ref = ref 0. in
  for i = 1 to nbTime do
    tree := avl_rnd_create(nbMax, int_of_float((2. ** power)));
    time := Sys.time();
    tree := ajt_avl((nbMax+200),!tree);
    time := Sys.time() -. !time;
    somme := !somme +. !time;
  done;
  ((!somme)/.float_of_int(nbTime))
;;

  
averageTimePerPowerAdd(1000,0.,1000);;
averageTimePerPowerAdd(1000,1.,1000);;
averageTimePerPowerAdd(1000,2.,1000);;
averageTimePerPowerAdd(1000,3.,1000);;
averageTimePerPowerAdd(1000,4.,1000);;
averageTimePerPowerAdd(1000,5.,1000);;
averageTimePerPowerAdd(1000,6.,1000);;
averageTimePerPowerAdd(1000,7.,1000);;
averageTimePerPowerAdd(1000,8.,1000);;
averageTimePerPowerAdd(1000,9.,1000);;
averageTimePerPowerAdd(1000,10.,1000);;





(*Suppression*)

let averageTimePerPowerSup (nbMax,power,nbTime : int *float*int) : float  =
  let tree : int avl ref = ref (empty()) in
  let time : float ref = ref 0. in
  let somme : float ref = ref 0. in
  for i = 1 to nbTime do
    tree := avl_rnd_create(nbMax, (int_of_float((2. ** power)))-1);
    tree := ajt_avl((nbMax+200),!tree);
    time := Sys.time();
    tree := sup_avl((nbMax+200),!tree);
    time := Sys.time() -. !time;
    somme := !somme +. !time;
  done;
  ((!somme)/.float_of_int(nbTime))
;;

  
averageTimePerPowerSup(1000,0.,1000);;
averageTimePerPowerSup(1000,1.,1000);;
averageTimePerPowerSup(1000,2.,1000);;
averageTimePerPowerSup(1000,3.,1000);;
averageTimePerPowerSup(1000,4.,1000);;
averageTimePerPowerSup(1000,5.,1000);;
averageTimePerPowerSup(1000,6.,1000);;
averageTimePerPowerSup(1000,7.,1000);;
averageTimePerPowerSup(1000,8.,1000);;
averageTimePerPowerSup(1000,9.,1000);;
averageTimePerPowerSup(1000,10.,1000);;





(*Recherche*)
let averageTimePerPowerSeek (nbMax,power,nbTime : int *float*int) : float  =
  let tree : int avl ref = ref (empty()) in
  let time : float ref = ref 0. in
  let somme : float ref = ref 0. in
  for i = 1 to nbTime do
    tree := avl_rnd_create(nbMax, (int_of_float((2. ** power)))-1);
    tree := ajt_avl((nbMax+200),!tree);
    time := Sys.time();
    tree := seek_avl((nbMax+200),!tree);
    time := Sys.time() -. !time;
    somme := !somme +. !time;
  done;
  ((!somme)/.float_of_int(nbTime))
;;

  
averageTimePerPowerSeek(1000,0.,1000);;
averageTimePerPowerSeek(1000,1.,1000);;
averageTimePerPowerSeek(1000,2.,1000);;
averageTimePerPowerSeek(1000,3.,1000);;
averageTimePerPowerSeek(1000,4.,1000);;
averageTimePerPowerSeek(1000,5.,1000);;
averageTimePerPowerSeek(1000,6.,1000);;
averageTimePerPowerSeek(1000,7.,1000);;
averageTimePerPowerSeek(1000,8.,1000);;
averageTimePerPowerSeek(1000,9.,1000);;
averageTimePerPowerSeek(1000,10.,1000);;


(*2.2.2*)

(*Fonction de reéquilibrage d'un AVL*)
let reequilibrer (tree : 'a avl) : 'a avl * int =
  
  let e : int = getEquilibre(tree) in
  match e with
    
  (*Cas déséquilibre gauche*)
  | 2 -> let eg = getEquilibre(lson(tree)) in
         (
           match eg with
           | 1 -> rd(tree),1
           | 0 -> rd(tree),1
           | -1 -> rgd(tree),1
           | _ -> tree,0
         )
         
  (*Ca déséquilibre droit*)       
  | -2 -> let ed = getEquilibre(rson(tree)) in
          (
            match ed with
            | 1 -> rdg(tree),1
            | 0 -> rg(tree),1
            | -1 -> rg(tree),1
            | _ -> tree,0
          )
  (*Autres cas*)
  | _ -> tree,0
;;

let rec ajt_avl(elem, tree : 'b * 'a avl) : 'a avl * int=

  if (isEmpty(tree))
  then rooting((1, 0, elem), empty(), empty()),0
  else
    (
      let ((h, e, r), g, d) =(root(tree), lson(tree), rson(tree)) in
      
      (*Cas element inférieur racine*)
      if (elem < r)
      then let newG,e = ajt_avl(elem, g) in
           let newH = cptMaxH(newG, d) + 1 in
           let newE = cptEquilibre(newG, d) in
           let ans,e1 = reequilibrer(rooting((newH, newE, r), newG, d)) in
		   ans,(e+e1)
      else (*Cas élément supérieur racine*)
        if (elem > r)
        then let newD,e = ajt_avl(elem, d) in
             let newH = cptMaxH(g, newD)+1 in
             let newE = cptEquilibre(g, newD) in
             let ans,e1 = reequilibrer(rooting((newH, newE, r), g, newD)) in
             ans,(e+e1)
			 
        else tree,0 (*Cas élément égal*)
    )
;;

let averageRotation(mode,nbList,nbElement,nbMax,nbTime : int * int * int * int * int) : int * int=
  let rotation : int ref = ref 0 in
  let nbE = ref nbElement in
  let tree = ref (empty()) in
  let nb_valeur = ref 0 in
  if mode> -2 && mode <3
  then
    (
      for i = 1 to nbTime do
        for j = 1 to nbList do
          let randomV = ref (Random.int nbMax) in
          if(mode = 2)
          then nbE := Random.int (nbElement+1);
          for k = 1 to !nbE do
            let newT,r = ajt_avl(!randomV,!tree) in
            tree := newT;
            randomV := !randomV + Random.int 100;
            rotation := !rotation + r;
            nb_valeur := !nb_valeur +1;
          done; 
          nbE := !nbE + mode;
        done;
        nbE := nbElement;
        tree := empty();
      done;
       
      (!rotation/nbTime),(!nb_valeur/nbTime)
    )
  else failwith "Error, wrong mode (averageRotationPerPower)" 
;;

averageRotation(-1,1000,10,10000000,50);;
averageRotation(0,1000,10,10000000,50);;
averageRotation(1,1000,10,10000000,50);;
averageRotation(2,1000,10,10000000,50);;
