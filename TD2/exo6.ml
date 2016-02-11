type couleur = Pique | Coeur | Carreau | Trefle;;

type carte =
| As of couleur
| Roi of couleur
| Dame of couleur
| Valet of couleur
| Petite of couleur * int ;;

let valeur carte coul_atout =
    match carte with
    | As _ -> 11
    | Roi _ -> 4
    | Dame _ -> 3
    | Valet c when c = coul_atout -> 20
    | Valet _ -> 2
    | Petite (_,10) -> 10
    | Petite (c,9) when c = coul_atout -> 14
    | Petite (_,_) -> 0
;;

let carte = As Coeur in
    print_int (valeur carte Coeur);
    print_newline ()
    ;;

let carte =Valet Coeur in
    print_int (valeur carte Coeur);print_newline ();;

let carte = Petite (Coeur,9) in
    print_int (valeur carte Coeur);
    print_newline ()
    ;;
