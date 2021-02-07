(* === Projet Algorithmique et Programmation 3ème année === *)
(* ====== Auteurs : Fréjoux Gaëtan && Niord Mathieu ======= *)


open Btree;;
open List;;


(*TYPE*)

type 'a bs_tree = 'a t_btree;;
(*Fonction qui permet de savoir si la valeur passée en paramètre est dans
  l'arbre de recherche*)

(*FONCTIONS UTILES*)

let show_int_bst(t : 'a bs_tree) = show_int_btree(t : 'a t_btree);;
let t1 : 'a bs_tree = rooting(3, empty(), empty());;


let rec bst_seek(bst,value : 'a bs_tree * int) :'a bs_tree =
  if (isEmpty(bst))
  then empty()
  else
    if(root(bst) = value)
    then bst
    else
      if(root(bst) > value)
      then bst_seek(lson(bst), value)
      else bst_seek(rson(bst), value)
;;


let rec bst_linsert(b,v : 'a bs_tree * int) : 'a bs_tree =
  if (isEmpty(b))
  then
    rooting(v,empty(),empty())
  else
    let (r, fg, fd) : ('a * 'a bs_tree * 'a bs_tree) =
      (root(b), lson(b), rson(b)) in
    if (v <= r)
    then rooting(r, bst_linsert(fg, v), fd)
    else rooting(r, fg, bst_linsert(fd, v))
;;


let rec bst_lbuild(l : int list) :'a bs_tree =
  match l with
  | [] -> empty();
  | hd::tl -> bst_linsert(bst_lbuild(tl), hd)
;;


let isLeaf(b : 'a bs_tree) : bool =
  (isEmpty(lson(b))) && (isEmpty(rson(b)))
;;


let hasLson(b : 'a bs_tree) : bool =
  not(isEmpty(lson(b)))
;;


let hasRson(b : 'a bs_tree) : bool =
  not(isEmpty(rson(b)))
;;


let rec bst_get_left_val(b : 'a bs_tree) : 'a * 'a bs_tree =
  if (not(hasLson(b)))
  then (root(b),rson(b))
  else let a, c = bst_get_left_val(lson(b)) in
       a,rooting(root(b), c, rson(b))    
;;


let rec bst_delete(b, v : 'a bs_tree * 'a) : 'a bs_tree * bool =
  if (isEmpty(b))
  then
    (b, false)
  else
    if(root(b) = v)
    then
      if (isLeaf(b))
      then (empty(), true)
      else
        if (hasLson(b) && not(hasRson(b)))
        then
          (lson(b), true)
        else
          if (not(hasLson(b)) && hasRson(b))
          then
            (rson(b), true)
          else
            let r,a = bst_get_left_val(rson(b)) in
            (rooting(r, lson(b), a), true)
    else
      if (root(b) > v)
      then
        let(newl,bo) = bst_delete(lson(b),v) in
        (rooting(root(b), newl, rson(b)), bo)
      else
        let(newl,bo) = bst_delete(rson(b),v) in
        (rooting(root(b), lson(b), newl), bo)
;;


(*END OF FILE*)
