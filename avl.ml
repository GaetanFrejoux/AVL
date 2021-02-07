(* === Projet Algorithmique et Programmation 3ème année === *)
(* ====== Auteurs : Fréjoux Gaëtan && Niord Mathieu ======= *)


#use "abr.ml";;


(*** === DEUXIÈME PARTIE : Les AVL === ***)



(* === QUESTION 1 : Implantation d'un module AVL === *)


(*TYPE*)

(*Nous avons décider de stocker le déséquilibre et la hauteur pour
  réaliser des fonctions plus simple ainsi que moins complexe.*)

(*On a un triplet avec une première valeur qui correspond
  à la hauteur de l'arbre et une 2ème au déséquilibre et la 3ème à la valeur*)
type 'a avl = (int * int * 'a) t_btree;;


(*FONCTIONS UTILES*)

(*Fonction d'affichage d'un AVL*)
let show_int_avl (tree : int avl) =
  show((fun (h, e, r : int * int * int) ->
      String.concat "," [(string_of_int h); (string_of_int e); (string_of_int r)]), tree)
;;

(*Fonction qui récupère la valeur de la hauteur*)
let getHeight(tree : 'a avl) : int =
  if (isEmpty(tree))
  then 0
  else let (h, e, r) : (int * int * 'a) = root(tree) in h
;;

(*Fonction qui récupère la valeur de l'equilibre*)
let getEquilibre(tree : 'a avl) : int =
  if (isEmpty(tree))
  then 0
  else let (h, e, r) : (int * int * 'a) = root(tree) in e
;;

(*Fonction qui récupère la valeur de la racine*)
let getRoot(tree : 'a avl) : 'a =
  if (isEmpty(tree))
  then failwith "Tree is empty (getRoot)"
  else let (h, e, r) : (int * int * 'a) = root(tree) in r
;;

(*Fonction qui calcule le maximum des hauteurs entre deux arbres.
  Cette fonction sera utilisé notamment pour recalculer la hauteur lors
  d'ajout ou suppresion de valeur *)
let cptMaxH(left, right :'a avl * 'a avl) : int = (max (getHeight(left)) (getHeight(right)));;

(*Fonction qui calcule l'equilibre entre deux arbre.
  Cette fonction sera utilisé notamment pour recalculer la hauteur lors
  d'ajout ou suppresion de valeur *)
let cptEquilibre(left, right: 'a avl * 'a avl) : int = (getHeight(left)) - (getHeight(right));;


(***QUESTION 1.1***)

(*FONCTIONS*)

(*Fonction de rotation gauche*)
let rg(tree : 'a avl) : 'a avl =

  if (not(isEmpty(tree)) && not(isEmpty(rson(tree))))
  then let ((h, e, r), g, d) = (root(tree), lson(tree), rson(tree)) in

    (*Création du nouveau fils gauche*)
    let gH = cptMaxH(g, lson(d)) + 1 and
        gE = cptEquilibre(g, lson(d)) in
    let newG = rooting((gH, gE, r), g, lson(d)) in

    (*Création du nouvel arbre*)
    let newH = cptMaxH(newG, rson(d)) + 1 and
        newE = cptEquilibre(newG, rson(d)) in
    rooting((newH, newE, getRoot(d)), newG, rson(d))

  else failwith "Error, cannot rotate this tree (rg)"
;;

(*Fonction de rotation droite*)
let rd(tree : 'a avl) : 'a avl =

  if (not(isEmpty(tree)) && not(isEmpty(lson(tree))))
  then let ((h, e, r), g, d) = (root(tree), lson(tree), rson(tree)) in

    (*Création du nouveau fils droit*)
    let dH = cptMaxH(rson(g), d) + 1 and
        dE = cptEquilibre(rson(g), d) in
    let newD = rooting((dH, dE, r), rson(g), d)in

    (*Création du nouvel arbre*)
    let newH = cptMaxH(lson(g), newD) + 1 and
        newE = cptEquilibre(lson(g), newD) in
    rooting((newH, newE, getRoot(g)), lson(g), newD)

  else failwith "Error, cannot rotate this tree (rd)"
;;

(*Fonction de double rotation droite gauche*)
let rdg(tree : 'a avl) : 'a avl =

  let (r, g, d) : 'b * 'a avl * 'a avl = (root(tree), lson(tree), rson(tree)) in
  
  (*Première rotation*)
  let tmp : 'a avl = rooting(r, g, rd(d)) in
  
  (*Seconde rotation*)
  let res : 'a avl = rg(tmp) in res  
;;

(*Fonction de double rotation gauche droite*)
let rgd(tree : 'a avl) : 'a avl =

  let (r, g, d) : 'b * 'a avl * 'a avl = (root(tree), lson(tree), rson(tree)) in

  (*Première rotation*)
  let tmp : 'a avl = rooting(r, rg(g), d) in

  (*Seconde rotation*)
  let res : 'a avl = rd(tmp) in res 
;;


(***QUESTION 1.2***)

(*FONCTIONS*)

(*Fonction de reéquilibrage d'un AVL*)
let reequilibrer (tree : 'a avl) : 'a avl =
  
  let e : int = getEquilibre(tree) in
  match e with
    
  (*Cas déséquilibre gauche*)
  | 2 -> let eg = getEquilibre(lson(tree)) in
         (
           match eg with
           | 1 -> rd(tree)
           | 0 -> rd(tree)
           | -1 -> rgd(tree)
           | _ -> tree
         )
         
  (*Ca déséquilibre droit*)       
  | -2 -> let ed = getEquilibre(rson(tree)) in
          (
            match ed with
            | 1 -> rdg(tree)
            | 0 -> rg(tree)
            | -1 -> rg(tree)
            | _ -> tree
          )
  (*Autres cas*)
  | _ -> tree
;;


(***QUESTION 1.3***)

(*FONCTIONS*)

let rec ajt_avl(elem, tree : 'b * 'a avl) : 'a avl =

  if (isEmpty(tree))
  then rooting((1, 0, elem), empty(), empty())
  else
    (
      let ((h, e, r), g, d) =(root(tree), lson(tree), rson(tree)) in
      
      (*Cas element inférieur racine*)
      if (elem < r)
      then let newG = ajt_avl(elem, g) in
           let newH = cptMaxH(newG, d) + 1 in
           let newE = cptEquilibre(newG, d) in
		   
           reequilibrer(rooting((newH, newE, r), newG, d));
		   
      else (*Cas élément supérieur racine*)
        if (elem > r)
        then let newD = ajt_avl(elem, d) in
             let newH = cptMaxH(g, newD)+1 in
             let newE = cptEquilibre(g, newD) in
             reequilibrer(rooting((newH, newE, r), g, newD))
			 
        else tree (*Cas élément égal*)
    )
;;

(*Fonction qui renvoie l'élément le plus grand d'un AVL*)
let rec max_avl(tree : 'a avl) : 'a =

  if (isEmpty(tree))
  then failwith "Tree is empty (max_avl)"
  else let (r,d) = (getRoot(tree),rson(tree)) in

       if (isEmpty(d))
       then r
       else max_avl(d)
;;

(*Fonction qui renvoie l'arbre sans sa valeur maximal*)
let rec dmax_avl(tree : 'a avl) : 'a avl =
  if (isEmpty(tree))
  then failwith "Tree is empty (dmax_avl)"
  else let (g, d) = (lson(tree), rson(tree))in
       if (isEmpty(d))
       then g
       else let newD = dmax_avl(d) in
            let newH = cptMaxH(g, newD) + 1 and
                newE = cptEquilibre(g, newD) in
				
          reequilibrer(rooting((newH, newE, getRoot(tree)), g, newD))
;;

(*Fonction de suppresion d'une valeur*)
let rec sup_avl(elem, tree : 'a * 'a avl) : 'a avl =

  if (isEmpty(tree))
  then tree
  else
    let ((h, e, r), g, d) = (root(tree), lson(tree), rson(tree)) in

    (*Cas element inférieur à la racine*)
    if (elem < r)
    then let newG = sup_avl(elem, g) in
         let newH = cptMaxH(newG, d)+1 and
             newE = cptEquilibre(newG, d) in
         reequilibrer(rooting((newH, newE, r), newG, d))
    else

      (*Cas element superieur à la racine*)
      if (elem > r)
      then let newD = sup_avl(elem, d) in
           let newH = cptMaxH(g, newD) + 1 and
               newE = cptEquilibre(g, newD) in
	   
           reequilibrer(rooting((newH, newE, r), g, newD))
      else

        (*Cas element égal à la racine*)
        if (isEmpty(g))
        then (
          if (isEmpty(d))
          then empty()
          else d
        )
        else
          if (isEmpty(d))
          then g
          else let newG = dmax_avl(g) in
	       
               let newH = cptMaxH(newG, d) + 1 and
                   newE = cptEquilibre(newG, d) in
               reequilibrer(rooting((newH, newE, max_avl(g)), newG, d))
;;


(***QUESTION 1.4***)

(*FONCTIONS*)

(*Fonction qui recherche une valeur donnée dans un AVL*)
let rec seek_avl(elem, tree : 'a  * 'a avl) : 'a avl =
  
  if (isEmpty(tree) || getRoot(tree) = elem)
  then tree
  else
    if (getRoot(tree) > elem)
    then seek_avl(elem, lson(tree))
    else seek_avl(elem, rson(tree))
;;



(* === QUESTION 2 : Expérimentations sur les AVL === *)
(* voir le fichier avl_experimentation.ml *)
(*Fonction qui génère un avl de valeurs aléatoires bornées avec une taille donné*)
let rec avl_rnd_create(nbMax, size : int * int) : 'a avl =
  if(size = 0)
  then empty()
  else let t = avl_rnd_create(nbMax, size-1) in
       ajt_avl(Random.int nbMax,t)
;;


(*END OF FILE*)
