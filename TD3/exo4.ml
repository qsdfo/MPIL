(* TME3 probleme (exercice 2 page 60 du mulet + modifs : Arbres Lexicaux) *)

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

(* 2 *)
exception Deja_defini of string ;;

let ajoute m d =
  let l = String.length m in
  let rec aux r d =
    match r, d with
      | 1, Lettre (c, b, a) :: tl when c = m.[l - r] ->
	  (* cas final si prefixe *)
	  if b then
	    raise (Deja_defini m)
	  else
	    Lettre (c, true, a) :: tl
      | 1, [] ->
	  (* cas final si pas de prefixe *)
	  Lettre (m.[l - 1], true, []) :: []
      | _, (Lettre (c, b, a) as e) :: tl ->
	  (* parcours si prefixe *)
	  if c = m.[l - r] then
	    Lettre (c, b, aux (r - 1) a) :: tl
	  else
	    e :: aux r tl
      | _, [] ->
	  (* parcours si pas de prefixe *)
	  Lettre (m.[l - r], false, aux (r - 1) []) :: []
  in
    aux l d
;;

(* 3 *)
let construit l =
  let rec aux l d = match l with
      [] -> d
    | t::q -> aux q (ajoute t d)
  in aux l [] ;;


(* 4 *)
let string_of_char c = String.make 1 c ;;

let rec list_of_tree d = match d with
    [] -> []
  | (Lettre(c,b,l))::q when b ->
      let r1 = list_of_tree l
      and r2 = list_of_tree q in
      let pr = List.map (function s -> (string_of_char c)^s) r1 in
        ((string_of_char c)::pr)@r2
  | (Lettre(c,b,l))::q ->
      let r1 = list_of_tree l
      and r2 = list_of_tree q in
      let pr = List.map (function s -> (string_of_char c)^s) r1 in
        pr@r2 ;;

(* 6 *)

let affiche dic =
  let dic = list_of_tree dic in
  let rec aux = function
    [] -> ()
  | t::q -> print_string (t^"\n"); aux q
  in
    aux dic
;;

(* 7 *)

let rec boucle d =
  let command =
    print_string "> " ; flush stdout ;
    Str.split (Str.regexp "[ \t]+") (read_line ())
  in
    match command with
      | [ "affiche" ] ->
	  affiche d;
	  boucle d
      | [ "ajoute" ; m ] ->
	  begin
	    try
	      boucle (ajoute m d)
	    with Deja_defini _ ->
	      print_string "Erreur:  deja defini.\n" ;
	      boucle d
	  end
      | [ "existe" ; m ] ->
	  if existe m d then
	    print_string "oui\n"
	  else
	    print_string "non\n" ;
	  boucle d
      | [ "quitte" ] ->
	  ()
      | _ ->
	  print_string "Erreur: commande inconnue ou mal ecrite.\n" ;
	  boucle d
;;

print_endline "Bienvenue !" ;;
boucle [] ;;
