(* === Projet Algorithmique et Programmation 3�me ann�e === *)
(* ====== Auteurs : Fr�joux Ga�tan && Niord Mathieu ======= *)


#use "abr.ml";;


(*** === DEUXI�ME PARTIE : Les AVL === ***)



(* === QUESTION 1 : Implantation d'un module AVL === *)


(*TYPE*)

(*Nous avons d�cider de stocker le d�s�quilibre et la hauteur pour
  r�aliser des fonctions plus simple ainsi que moins complexe.*)

(*On a un triplet avec une premi�re valeur qui correspond
  � la hauteur de l'arbre et une 2�me au d�s�quilibre et la 3�me � la valeur*)
type 'a avl = (int * int * 'a) t_btree;;


(*FONCTIONS UTILES*)

(*Fonction d'affichage d'un AVL*)
let show_int_avl (tree : int avl) =
  show((fun (h, e, r : int * int * int) ->
      String.concat "," [(string_of_int h); (string_of_int e); (string_of_int r)]), tree)
;;

(*Fonction qui r�cup�re la valeur de la hauteur*)
let getHeight(tree : 'a avl) : int =
  if (isEmpty(tree))
  then 0
  else let (h, e, r) : (int * int * 'a) = root(tree) in h
;;

(*Fonction qui r�cup�re la valeur de l'equilibre*)
let getEquilibre(tree : 'a avl) : int =
  if (isEmpty(tree))
  then 0
  else let (h, e, r) : (int * int * 'a) = root(tree) in e
;;

(*Fonction qui r�cup�re la valeur de la racine*)
let getRoot(tree : 'a avl) : 'a =
  if (isEmpty(tree))
  then failwith "Tree is empty (getRoot)"
  else let (h, e, r) : (int * int * 'a) = root(tree) in r
;;

(*Fonction qui calcule le maximum des hauteurs entre deux arbres.
  Cette fonction sera utilis� notamment pour recalculer la hauteur lors
  d'ajout ou suppresion de valeur *)
let cptMaxH(left, right :'a avl * 'a avl) : int = (max (getHeight(left)) (getHeight(right)));;

(*Fonction qui calcule l'equilibre entre deux arbre.
  Cette fonction sera utilis� notamment pour recalculer la hauteur lors
  d'ajout ou suppresion de valeur *)
let cptEquilibre(left, right: 'a avl * 'a avl) : int = (getHeight(left)) - (getHeight(right));;


(***QUESTION 1.1***)

(*FONCTIONS*)

(*Fonction de rotation gauche*)
let rg(tree : 'a avl) : 'a avl =

  if (not(isEmpty(tree)) && not(isEmpty(rson(tree))))
  then let ((h, e, r), g, d) = (root(tree), lson(tree), rson(tree)) in

    (*Cr�ation du nouveau fils gauche*)
    let gH = cptMaxH(g, lson(d)) + 1 and
        gE = cptEquilibre(g, lson(d)) in
    let newG = rooting((gH, gE, r), g, lson(d)) in

    (*Cr�ation du nouvel arbre*)
    let newH = cptMaxH(newG, rson(d)) + 1 and
        newE = cptEquilibre(newG, rson(d)) in
    rooting((newH, newE, getRoot(d)), newG, rson(d))

  else failwith "Error, cannot rotate this tree (rg)"
;;

(*Fonction de rotation droite*)
let rd(tree : 'a avl) : 'a avl =

  if (not(isEmpty(tree)) && not(isEmpty(lson(tree))))
  then let ((h, e, r), g, d) = (root(tree), lson(tree), rson(tree)) in

    (*Cr�ation du nouveau fils droit*)
    let dH = cptMaxH(rson(g), d) + 1 and
        dE = cptEquilibre(rson(g), d) in
    let newD = rooting((dH, dE, r), rson(g), d)in

    (*Cr�ation du nouvel arbre*)
    let newH = cptMaxH(lson(g), newD) + 1 and
        newE = cptEquilibre(lson(g), newD) in
    rooting((newH, newE, getRoot(g)), lson(g), newD)

  else failwith "Error, cannot rotate this tree (rd)"
;;

(*Fonction de double rotation droite gauche*)
let rdg(tree : 'a avl) : 'a avl =

  let (r, g, d) : 'b * 'a avl * 'a avl = (root(tree), lson(tree), rson(tree)) in
  
  (*Premi�re rotation*)
  let tmp : 'a avl = rooting(r, g, rd(d)) in
  
  (*Seconde rotation*)
  let res : 'a avl = rg(tmp) in res  
;;

(*Fonction de double rotation gauche droite*)
let rgd(tree : 'a avl) : 'a avl =

  let (r, g, d) : 'b * 'a avl * 'a avl = (root(tree), lson(tree), rson(tree)) in

  (*Premi�re rotation*)
  let tmp : 'a avl = rooting(r, rg(g), d) in

  (*Seconde rotation*)
  let res : 'a avl = rd(tmp) in res 
;;


(***QUESTION 1.2***)

(*FONCTIONS*)

(*Fonction de re�quilibrage d'un AVL*)
let reequilibrer (tree : 'a avl) : 'a avl =
  
  let e : int = getEquilibre(tree) in
  match e with
    
  (*Cas d�s�quilibre gauche*)
  | 2 -> let eg = getEquilibre(lson(tree)) in
         (
           match eg with
           | 1 -> rd(tree)
           | 0 -> rd(tree)
           | -1 -> rgd(tree)
           | _ -> tree
         )
         
  (*Ca d�s�quilibre droit*)       
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
      
      (*Cas element inf�rieur racine*)
      if (elem < r)
      then let newG = ajt_avl(elem, g) in
           let newH = cptMaxH(newG, d) + 1 in
           let newE = cptEquilibre(newG, d) in
		   
           reequilibrer(rooting((newH, newE, r), newG, d));
		   
      else (*Cas �l�ment sup�rieur racine*)
        if (elem > r)
        then let newD = ajt_avl(elem, d) in
             let newH = cptMaxH(g, newD)+1 in
             let newE = cptEquilibre(g, newD) in
             reequilibrer(rooting((newH, newE, r), g, newD))
			 
        else tree (*Cas �l�ment �gal*)
    )
;;

(*Fonction qui renvoie l'�l�ment le plus grand d'un AVL*)
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

    (*Cas element inf�rieur � la racine*)
    if (elem < r)
    then let newG = sup_avl(elem, g) in
         let newH = cptMaxH(newG, d)+1 and
             newE = cptEquilibre(newG, d) in
         reequilibrer(rooting((newH, newE, r), newG, d))
    else

      (*Cas element superieur � la racine*)
      if (elem > r)
      then let newD = sup_avl(elem, d) in
           let newH = cptMaxH(g, newD) + 1 and
               newE = cptEquilibre(g, newD) in
	   
           reequilibrer(rooting((newH, newE, r), g, newD))
      else

        (*Cas element �gal � la racine*)
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

(*Fonction qui recherche une valeur donn�e dans un AVL*)
let rec seek_avl(elem, tree : 'a  * 'a avl) : 'a avl =
  
  if (isEmpty(tree) || getRoot(tree) = elem)
  then tree
  else
    if (getRoot(tree) > elem)
    then seek_avl(elem, lson(tree))
    else seek_avl(elem, rson(tree))
;;



(* === QUESTION 2 : Exp�rimentations sur les AVL === *)
(* voir le fichier avl_experimentation.ml *)
(*Fonction qui g�n�re un avl de valeurs al�atoires born�es avec une taille donn�*)
let rec avl_rnd_create(nbMax, size : int * int) : 'a avl =
  if(size = 0)
  then empty()
  else let t = avl_rnd_create(nbMax, size-1) in
       ajt_avl(Random.int nbMax,t)
;;


(*END OF FILE*)
