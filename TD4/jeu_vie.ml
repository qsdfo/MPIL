#load "graphics.cma";;

let array_sum a =
  let r = ref 0 in
    for i = 0 to Array.length a - 1 do
      r := !r + a.(i)
    done ;
    !r
;;

let list_sum l =
  let r = ref 0 in
  let lr = ref l in
    while !lr <> [] do
      r := !r + List.hd !lr ;
      lr := List.tl !lr
    done ;
    !r
;;

let array_iter f a =
  for i = 0 to Array.length a - 1 do
    f a.(i)
  done
;;

let list_iter f l =
  let lr = ref l in
    while !lr <> [] do
      f (List.hd !lr) ;
      lr := List.tl !lr
    done
;;


type gen = bool array array ;;
let f = false and t = true
let init_gen = [|
  [| f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f |] ;
  [| f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f |] ;
  [| f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f |] ;
  [| f ; f ; f ; f ; t ; t ; t ; f ; f ; f ; t ; t ; t ; f ; f ; f ; f |] ;
  [| f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f |] ;
  [| f ; f ; t ; f ; f ; f ; f ; t ; f ; t ; f ; f ; f ; f ; t ; f ; f |] ;
  [| f ; f ; t ; f ; f ; f ; f ; t ; f ; t ; f ; f ; f ; f ; t ; f ; f |] ;
  [| f ; f ; t ; f ; f ; f ; f ; t ; f ; t ; f ; f ; f ; f ; t ; f ; f |] ;
  [| f ; f ; f ; f ; t ; t ; t ; f ; f ; f ; t ; t ; t ; f ; f ; f ; f |] ;
  [| f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f |] ;
  [| f ; f ; f ; f ; t ; t ; t ; f ; f ; f ; t ; t ; t ; f ; f ; f ; f |] ;
  [| f ; f ; t ; f ; f ; f ; f ; t ; f ; t ; f ; f ; f ; f ; t ; f ; f |] ;
  [| f ; f ; t ; f ; f ; f ; f ; t ; f ; t ; f ; f ; f ; f ; t ; f ; f |] ;
  [| f ; f ; t ; f ; f ; f ; f ; t ; f ; t ; f ; f ; f ; f ; t ; f ; f |] ;
  [| f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f |] ;
  [| f ; f ; f ; f ; t ; t ; t ; f ; f ; f ; t ; t ; t ; f ; f ; f ; f |] ;
  [| f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f |] ;
  [| f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f |] ;
  [| f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f ; f |] ;
|] ;;

let neighbours g x y =
  List.fold_left
    (fun r (x,y) ->
       try
	 if g.(y).(x) then r + 1 else r
       with Invalid_argument "index out of bounds" -> r)
    0
    [ x - 1, y - 1 ; x, y - 1 ; x + 1, y - 1 ;
      x - 1, y ;                x + 1, y ;
      x - 1, y + 1 ; x, y + 1 ; x + 1, y + 1 ]
;;

let next_gen g =
  let h = Array.length g in
  let w = Array.length g.(0) in
  let ng = Array.map Array.copy g in
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
	ng.(y).(x) <-
	  match neighbours g x y with
	    | 2 -> g.(y).(x)
	    | 3 -> true
	    | _ -> false
      done
    done ; ng
;;

let init_graph g =
  Graphics.open_graph
    (Printf.sprintf " %dx%d"
       (Array.length g.(0) * 10)
       (Array.length g * 10))
;;

let draw_gen g =
  Graphics.clear_graph () ;
  for y = 0 to Array.length g - 1 do
    for x = 0 to Array.length g.(0) - 1 do
      if g.(y).(x) then
	Graphics.fill_rect (x * 10) (y * 10) 10 10
    done
  done
;;

let continue () =
  let ev = Graphics.wait_next_event [ Graphics.Key_pressed ] in
    ev.Graphics.key <> 'q'
;;

let gen = ref init_gen in
  init_graph !gen ;
  draw_gen !gen ;
  while continue () do
    gen := next_gen !gen ;
    draw_gen !gen
  done ;
  Graphics.close_graph ()
;;
