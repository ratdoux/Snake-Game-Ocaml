
(* ------------------------ *)
(*        listes            *)
(* ------------------------ *)



let len(l) = List.length l;;

let fst(l) =
  match l with
    [] -> failwith "error fst : list is empty"
  | hd::_ -> hd
;;

let rec lst(l) =
  match l with
    [] -> failwith "error lst : list is empty"
    | hd::[] -> hd
    | _::tail -> lst(tail)
;;

let nth(l, k) = 
let rec nth1(l,k) =
  match l with
    []->  failwith "error nth : index must be in [0..len(l)-1]"
  | hd::tail -> if k = 0 then hd else nth1(tail,k-1)
in
if k < 0
then failwith "error  nth : index must be positive"
else nth1(l,k)
;;


let add_fst(l,e) = e::l ;;

let rec add_lst(l,e) =
  match l with
    [] -> [e]
  | hd::tail -> hd::add_lst(tail,e);;

let add_nth(l, e, k) =
let rec add_nth1(l, e, k) =
  match l with
    [] -> [e]
    | hd ::tail -> if k = 0 then e::l else hd::add_nth1(tail, e, k-1)
in 
if k < 0
then failwith "error add_nth : index k must be positive"
else
  if k > len(l)
  then failwith "error add_nth : index must be in [0..len(l)]"
  else add_nth1(l,e,k)
;;

let rem_fst(l) = 
  match l with
  [] -> failwith "error rem_fst : list is empty"
  | _::tail -> tail
;;

let rec rem_lst(l) =
  match l with
  [] -> failwith "error rem_lst : list is empty"
  | [x] -> []
  | x::tail -> x::rem_lst(tail)
 ;;

let rem_nth(l,k) =
let rec rem_nth1(l,k) =
  match l with
  | [] -> failwith "error rem_nth : index must be in [0..len(l)-1]"
  | hd:: tail -> if k = 0 then tail else hd::rem_nth1(tail, k-1)
in
if k < 0 
then failwith "error rem_nth :index k must be positive"
else rem_nth1(l,k)
;;

let concat(l1, l2) = l1 @ l2 ;;


(* ------------------------ *)
(*        tableaux          *)
(* ------------------------ *)

let arr_len(t) = Array.length t ;;

let arr_make(n, v) = Array.make n v ;;

type 'a matrix = 'a array array ;;

let mat_make(n, m, v) : 'a matrix = Array.make_matrix n m v ;;


(* aleatoire *)

let rand_init() = Random.self_init() ;;

let rand_init_expl(n) = Random.init(n) ;;

let rand_int_0(n) = Random.int(n+1) ;;

let rand_int(n, p) = Random.int(p-n + 1) + n ;;


(* ------------------------ *)
(*    lecture caractere     *)
(* ------------------------ *)

let read_char() : char =
  let s : string ref = ref "" and fin : bool ref = ref false in
    (
    while not(!fin) 
    do
      s:= read_line() ; 
      if String.length(!s) = 0
      then (print_string("erreur de saisie : vous n'avez saisi aucun caractère") ; print_newline() ;)
      else fin := true;
    done ;
    (!s).[0] ;
    )
;;


(* ------------------------ *)
(*    longueur string       *)
(* ------------------------ *)

let string_length(s : string) : int = String.length s ;;


(* ------------------------ *)
(*  pause durant execution  *)
(* ------------------------ *)

let wait(n : int) : unit =
  Unix.sleep (n)
;;



(* ------------------------ *)
(*        graphique         *)
(* ------------------------ *)

let open_graph(dx, dy : int * int) : unit = 
  if Sys.os_type = "Unix" then  
    let s = ":0 "^string_of_int(dx)^"x"^string_of_int(dy) in
      Graphics.open_graph s
  else
    let s = string_of_int(dx)^"x"^string_of_int(dy) in
      Graphics.open_graph s
;;

let close_graph() : unit = Graphics.close_graph() ;;

let clear_graph() : unit = Graphics.clear_graph() ;;

let resize_window(x, y) = Graphics.resize_window x y ;;



let moveto(x,y : int * int) : unit = Graphics.moveto x y ;;

let lineto(x, y : int * int) = Graphics.lineto x y ;;

let plot(x, y : int * int) = Graphics.plot x y ;;

let current_point() = Graphics.current_point() ;;

let draw_poly_line(t) = Graphics.draw_poly_line t ;;

let draw_circle(x, y, r : int * int * int) = Graphics.draw_circle x y r ;;

let draw_rect(x,y,dx,dy : int * int * int * int) : unit = 
  if Sys.os_type = "Unix" then  
    Graphics.draw_rect x y (dx- 1) (dy - 1)
  else
    Graphics.draw_rect x (y+1) (dx-1) (dy-1)
;;

let fill_rect(x,y,dx,dy : int * int * int * int) : unit = 
  if Sys.os_type = "Unix" then  
    Graphics.fill_rect x y (dx- 1) (dy - 1)
  else
    Graphics.fill_rect x y dx dy
;;

let fill_poly(t) = Graphics.fill_poly t ;;

let fill_circle(x, y, r : int * int * int) = Graphics.fill_circle x y r ;;



let set_line_width(e) = Graphics.set_line_width e ;;

(* 
let draw_str(s : string) : unit = Graphics.draw_string s ;;

let set_font(s : string) : unit = Graphics.set_font s ;; 
*)

let draw_string(s) = Graphics.draw_string s ;;

let set_text_size(n) = 
  let s = "-*-courier-medium-r-*-*-"^string_of_int(n)^"-*"
  in Graphics.set_font s ;;



type t_color = Graphics.color ;;


let set_color(color : t_color) = Graphics.set_color color ;;

let black : t_color = Graphics.black ;;
let blue : t_color = Graphics.blue ;;
let red : t_color = Graphics.red ;;
let green : t_color = Graphics.green ;;
let white : t_color = Graphics.white ;;
let yellow : t_color = Graphics.yellow ;;
let cyan : t_color = Graphics.cyan ;;
let magenta : t_color = Graphics.magenta ;;
let grey : t_color = 128 * 256 * 256 + 128 * 256 + 128 ;;



(* ------------------------ *)
(*   controle evenements    *)
(* ------------------------ *)

type t_CPstatus = Graphics.status ;;
type t_CPevent = Graphics.event ;;

let mouse_x(s : t_CPstatus) = s.mouse_x ;;
let mouse_y(s : t_CPstatus) = s.mouse_y ;;
let button(s : t_CPstatus) = s.button ;;
let keypressed(s : t_CPstatus) = s.keypressed ;;
let key(s : t_CPstatus) = s.key ;;

let button_down_event : t_CPevent = Graphics.Button_down ;;
let button_up_event : t_CPevent = Graphics.Button_up ;;
let key_pressed_event : t_CPevent = Graphics.Key_pressed ;;
let mouse_motion_event : t_CPevent = Graphics.Mouse_motion ;;
let poll_event : t_CPevent = Graphics.Poll ;;


let wait_next_event(l : t_CPevent list) : t_CPstatus =
  Graphics.wait_next_event(l)
;;

let loop_at_exit(l, f : t_CPevent list * (t_CPstatus -> unit)) : unit = 
  Graphics.loop_at_exit l f
;;

let key_pressed() : bool =
  Graphics.key_pressed()
;;

let read_key() : char =
  Graphics.read_key()
;;

let mouse_pos() : int * int =
  Graphics.mouse_pos()
;;

let button_down() : bool = 
  Graphics.button_down()
;;



                   

