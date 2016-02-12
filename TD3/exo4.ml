type noeud_lex = Lettre of char * bool * arbre_lex
and arbre_lex = noeud_lex  list;;

type mot = string;;

(* 1 *)
let existe m d =
  let l = String.length m in
  let rec aux r d =
    match r, d with
      | 1, Lettre (c, b, _) :: _ when c = m.[l - r] ->
	  (* cas final si prefixe *)
	  b
      | _, [] ->
	  (* cas final si pas de prefixe*)
	  false
      | _, Lettre (c, _, a) :: tl ->
	  (* choix largeur ou profondeur *)
	  if c = m.[l - r] then
	    aux (r - 1) a
	  else
	    aux r tl
  in
    aux l d
;;
