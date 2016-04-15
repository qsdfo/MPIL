(* EXO 1 *)

exception NeuronIncoherentInput;;

let rec neuronReaction x w t =
    let rec dotProduct x w =
        match (x,w) with
        ([],[]) -> 0
        | ([],_) -> raise NeuronIncoherentInput
        | (_,[]) -> raise NeuronIncoherentInput
        | (a::b,c::d) -> (a * c) + (dotProduct b d)
    in
    if (dotProduct x w) >= t
        then 1
        else 0
;;

let neuronReaction_List x w t =
    try
        let productList = List.map2 (fun a b -> a * b) x w in
        let dotProduct = List.fold_left (fun a b -> a + b) 0 productList in
        if dotProduct >= t
            then 1
            else 0
    with Invalid_argument e -> raise NeuronIncoherentInput
;;

let neuronReaction_Imp x w t =
    let n_x = (List.length x) in
    let n_w = (List.length w) in
    if (n_x <> n_w) then raise NeuronIncoherentInput;
    let dotProduct = ref 0 in
    for i=0 to (n_x-1) do
        dotProduct := !dotProduct + (List.nth x i) * (List.nth w i)
    done;
    if !dotProduct >= t
        then 1
        else 0
;;


(* Exo2 *)
module type Valeur =
sig
    type t
    val plus : t -> t -> t
    val mult : t -> t -> t
    val minus : t -> t -> t
    val div : t -> t -> t
    val zero : t
    val one : t
end

(* Exemple utilisation *)
module V:Valeur =
struct
    type t = float
    let plus a b = a +. b
    let minus a b = a -. b
    let mult a b = a *. b
    let div a b = a /. b
    let zero = 0.
    let one = 1.
end


module Vecteur (V:Valeur)=
struct
    let init n (v:V.t) = Array.make n v
    let dim x = Array.length x
    let neuron a b =
        if Array.length a <> Array.length b
            then invalid_arg "Different array lengths";
        let times v u = Array.mapi (fun i v_i -> (V.mult v_i u.(i))) v in
        Array.fold_left (V.plus) V.zero (times a b)
end;;

module Matrix (V:Valeur)=
struct
    let init n p = Array.make_matrix n p V.zero
    let identity n =
        let mat = Array.make_matrix n n V.zero in
        for i=0 to n-1 do
            mat.(i).(i) <- V.one
        done
    let transpose (m:V.t array array) =
        let s = Array.length m in
        for i=0 to s-1 do
            for j=0 to s-1 do
                let temp = m.(i).(j) in
                m.(i).(j) <- m.(j).(i);
                m.(j).(i) <- temp
            done;
        done
end;;

(* Exo3 *)
module PerceptronLayer =
struct
    let init n r =
        let mat = Array.make_matrix n r 0. in
        for i=0 to n do
            for j=0 to r do
                mat.(i).(j) <- (Random.float 1.)
            done;
        done;
        mat
    let neuron a b =
        if Array.length a <> Array.length b
            then invalid_arg "Different array lengths";
        let times v u = Array.mapi (fun i v_i -> v_i *. u.(i)) v in
        Array.fold_left (+.) 0. (times a b)
    let eval l e =
        let num_n = Array.length l in
        let result = Array.make num_n 0. in
        for i=0 to num_n do
            result.(i) <- neuron l.(i) e
        done;
        result
end

(*Exo4*)
let init n dim=
    let reseau = Array.make n (PerceptronLayer.init dim dim) in
    for i=0 to ((Array.length reseau)-1) do
        reseau.(i) <- (PerceptronLayer.init dim dim)
    done;
    reseau
;;

(* Exo5 *)
let propagate reseau x =
    let n_couche = Array.length reseau in
    let s = ref x in
    for i=0 to (n_couche-1) do
        s := PerceptronLayer.eval reseau.(i) !s
    done;
    !s
;;

(* print_int (neuronReaction [1; 2; 3] [1; 2; 1; 4] 9);; *)
