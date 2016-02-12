Random.self_init () ;;

type objet = Piece | Poulet | Eponge

type classe_perso = Archer | Barbare | Magicien

type perso = {
  classe : classe_perso ;
  xp : int ;
  sac : (objet * int) list ;
}

type race_monstre =
  Golem | Sanglier | Moustiques of int

type monstre = {
  race : race_monstre ;
  poche : objet ;
}

let rec ajoute_sac o s =
  match s with
    | [] -> [ (o, 1) ]
    | (so, n) :: tl when so = o -> (o, succ n) :: tl
    | e :: tl -> e :: ajoute_sac o tl

let print_sac (* l *) =
  List.iter
    (fun (o, n) ->
       Printf.printf
	 "%d %s%s\n"
	 n
	 (match o with
	    | Poulet -> "poulet"
	    | Piece -> "piece"
	    | Eponge -> "eponge")
	 (if n > 1 then "s" else "")
    ) (* l *)

let random_monstre () =
  { race =
      (match Random.int 3 with
	 | 0 -> Golem
	 | 1 -> Moustiques (Random.int 5 + 5)
	 | _ -> Sanglier) ;
    poche =
      (match Random.int 3 with
	 | 0 -> Piece
	 | 1 -> Poulet
	 | _ -> Eponge) ;
  }

  let frappe perso =
    let degats =
      match perso.classe with
        | Barbare -> 10
        | Archer -> 4
        | Magicien -> 5
    in
    let touche =
      Random.int 100 <
        (perso.xp +
  	 match perso.classe with
  	   | Archer -> 70
  	   | Magicien -> 50
  	   | Barbare -> 30)
    in
      if touche then degats else 0

  let frappe_monstre m =
    match m.race with
      | Golem -> 4
      | Moustiques nb -> nb / 2
      | Sanglier -> 2

exception Mort

let rec combat p m vp vm =
  let vm = vm - frappe p in
    if (p.classe = Archer) then print_string "YO" else print_string "CRAP"; print_newline ();
    if vm <= 0 then
      { p with
	  sac = ajoute_sac m.poche p.sac ;
	  xp = p.xp + 5 }
    else
      let vp = vp - frappe_monstre m in
	if vp <= 0 then
	  raise Mort
	else
    begin
        (* print_int vm;
    print_newline (); *)
	  combat p m vp vm;
     end
;;

let malheureuse_rencontre p =
  let m = random_monstre () in
    Printf.printf
      "Vous tombez nez a nez avec %s !\n"
      (match m.race with
	 | Golem ->
	     "un terrRRrrible golem"
	 | Moustiques nb ->
	     Printf.sprintf "une armee de %d moustiques" nb
	 | Sanglier ->
	     "un sanglier") ;
    combat p m 20 20


let rec jeu n p =
  try
    if n = 0 then (
      print_endline "Bravo, vous avez vaincu !" ;
      print_endline "Voici votre butin :" ;
      print_sac p.sac
    ) else
      jeu (n - 1) (malheureuse_rencontre p)
  with
    | Mort -> print_endline "vous etes mort !"
;;

jeu 5 { classe = Barbare ;
	xp = 0 ;
	sac = [] }
;;



let perso1 = {
    classe = Archer;
    sac = (Piece, 2)::[];
    xp = 30};;



combat perso1 (random_monstre ()) 100 10;;
