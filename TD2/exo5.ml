(* Testing the String module *)
(* print_int (String.length "Test string");; print_newline ();;
print_char (String.get "Test string" 0);; print_newline ();;
print_char (String.get "Test string" 5);; print_newline ();;
print_int (String.index "Test string" 'T');; print_newline ();;
print_int (String.index "Test string" 't');; print_newline ();; *)


(* Solution moche avec try/catch *)
let cherche_car char str =
    try
      String.index str char
    with
      Not_found -> -1 ;;

(*  Meilleure solution *)
let cherche_car_2 car str =
    let rec aux car str n =
    if n = String.length str then -1
    else if String.get str n = car then n
            else aux car str (n+1)
    in aux car str 0;;

(* print_int (cherche_car_2 'b' "Testouille");; print_newline ();;
print_int (cherche_car_2 't' "Testouille");; *)
