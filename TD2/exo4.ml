let liste1 = 1 :: [];;
let liste2 = 1 :: 1 :: [];;
let liste3 = 2 :: 1 :: [];;
let liste4 = 1 :: 2 :: 1 :: 1 :: [];;
let liste5 = 1 :: 1 :: 1 :: 2 :: 2 :: 1 :: [];;

let calcul_prefixe =
    let rec aux e l =
        match l with
            [] -> 0
            | h :: t -> if e = h then 1 + aux e t else 0
    in
        function
            [] -> 0
            | t :: q -> 1 + aux t q
;;

let rec genere_liste l =
    let rec remove_list arg =
        match arg with
            (_,[]) -> []
            | (0,h) -> h
            | (n,h::t) -> remove_list (n-1,t)
    in
    match l with
        [] -> []
        | h :: t -> let n = calcul_prefixe l in n :: h :: (genere_liste (remove_list (n, l)))
;;

let rec genere =
    function
    1 -> [[1]]
    | k -> let l = (genere (k-1)) in
                (genere_liste (List.hd l)) :: l
;;

let rec print_list = function
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l;;

let rec print_list_list = function
[] -> ()
| e::l -> print_list e ; print_newline () ; print_list_list l;;

print_list_list (genere 47);;

(* print_list (genere_liste liste1);;
print_newline ();;
print_list (genere_liste liste2);;
print_newline ();;
print_list (genere_liste liste3);;
print_newline ();;
print_list (genere_liste liste4);;
print_newline ();;
print_list (genere_liste liste5);; *)
