#use "snake.ml";;

let scoreint : int ref = ref 0;;

let score() : unit =
  
      (
        scoreint := !scoreint + 1;
        moveto(95, 610);
        set_color(color_of_value(EMPTY));
        fill_rect(95, 610, 50, 12);
        set_color(color_of_value(PROBLEM));
        draw_string(string_of_int(!scoreint)) ;
      )
;;

let compute_move_add(pos, dir, m : t_position * t_direction * t_matrix) : t_position * t_value =
  let new_pos : t_position = compute_new_position(pos, dir) in
  if
    new_pos.pt.x < 0 || new_pos.pt.x > (mymatrix_dx()-1) || new_pos.pt.y < 0 || new_pos.pt.y >  (mymatrix_dx()-1)
  then
    (new_pos, FRAME)
  else
    if dir = RIGHT 
    then
     (new_pos, m.(new_pos.pt.x-1).(new_pos.pt.y))
    else
      if dir = LEFT
      then
        (new_pos, m.(new_pos.pt.x+1).(new_pos.pt.y))
      else
        if dir = UP
        then
          (new_pos, m.(new_pos.pt.x).(new_pos.pt.y-1))
        else
          (new_pos, m.(new_pos.pt.x).(new_pos.pt.y+1))
          
;;

let add_snake_tail(pl : t_play) : unit =
  let newpos : t_position = {pt = {x = (lst(!(pl.sn))).pt.x; y = (lst(!(pl.sn))).pt.y}; dir = (lst(!(pl.sn))).dir} in
  pl.sn := add_lst(!(pl.sn), newpos);
  pl.mat.(newpos.pt.x-1).(newpos.pt.y) <- SNAKE;
  set_color(color_of_value(SNAKE));
  myplot(newpos.pt.x-1, newpos.pt.y);
;;

let add_bonus(pl : t_play) : unit =
  let pos : t_point = {x = Random.int(99); y = Random.int(99)} in
  
  pl.mat.(pos.x).(pos.y) <- BONUS;
  set_color(color_of_value(BONUS));
  myplot(pos.x, pos.y);
;;

let snakeToInit(pl: t_play) : unit =
 let snakelen = len(!(pl.sn)) in
  if snakelen > 5
  then
    (
     
     (
      for i = 5 to snakelen - 1
      do
        remove_snake_tail(pl);
      done;
     )
    )
  else
    ()
;;

let new_step(pl : t_play) : bool =
  let (newpos, value) : t_position * t_value = compute_move(fst(!(pl.sn)), analyze(fst(!(pl.sn))), pl.mat) in
  let (newposadd, valueadd) : t_position * t_value = compute_move_add(fst(!(pl.sn)), analyze(fst(!(pl.sn))), pl.mat) in
  
   move_snake(pl, newpos);
   (
     if !scoreint mod 5 = 0
     then
       (
       add_snake_newhead(pl, newposadd);
       add_bonus(pl);
       )
     else
       ()
   );
  
  if (value = EMPTY)
  then false
  else
    if (value = BONUS)
    then
      (
      snakeToInit(pl);
      false
      )
    else
      (
      if (value = FRAME) || (value = SNAKE)
      then
      (
        set_color(color_of_value(PROBLEM));
        myplot(newpos.pt.x, newpos.pt.y);
        set_color(black);
        moveto(95, 80);
        draw_string("GAME OVER");
      );
      true
      )

;;

let simulation() : unit =
  let pl : t_play = init_play() in
  let t : float ref = ref (Sys.time()) and newt : float ref = ref (Sys.time())  in
  let tacc : float ref = ref (Sys.time()) in 
  let thend : bool ref = ref false in
  
  scoreint := 0;
  
    (
    while not(!thend)
    do
      
      newt := Sys.time() ;
      
      
      while not((!newt -. !t) > !(pl.dt))
      do newt := Sys.time() ;
      done ; 
      t := !newt ; 
      handle_t_acc(t, tacc, pl) ;
      thend := new_step(pl) ;
      (
        score()
      )
    done ;
    )
;;

open_graph(699,699);;


let play() : (unit) =
  
  let thend : bool ref = ref false in
  let buttonDown : bool ref = ref false in
 open_graph(699,699);
 clear_graph();
 set_color(color_of_value(BONUS));
 myfill_rect(-1, 50, 100, 20);
 moveto(103,395);
 
 set_color(color_of_value(PROBLEM));
 (
   for i=0 to 11
   do
     draw_string("PLAY ");
   done;
 );
 
 set_color(color_of_value(PROBLEM));
 myfill_rect(10, 20, 80, 20);
 moveto(153,247);
 set_color(color_of_value(BONUS));
 (
   for i=0 to 9
   do
     draw_string("QUIT ");
   done;
 );
 

 while not(!thend)
 do
   let (i, j) : int * int = mouse_pos() in
   buttonDown := button_down();
 (
 if not(!buttonDown)
 then
   ()
  else
    if ((590 > i) && (i > 103)) && (( 450 > j) && (j > 352))
    then
      (
        thend := true;
        randombonus(50000);
      clear_graph();
      simulation()
      )
    else
      thend := false;
 
 )
 done
;;

clear_graph();;


  
play();;

let rec counttime(x: int) : int =

  if x < 10
  then
    
  (Unix.sleep(1);
   counttime((x+1))
  )
 else
   10
;;

#trace counttime;;

counttime(1);;

let randomcolor() : t_color =
  let a : t_color list = [black; blue; red; green; white; yellow; cyan; magenta; grey] in

  nth(a, Random.int(len(a)));;
    

let rec endgame(x : int) : unit =

  if x = 0
  then
  (set_color(randomcolor());
   myfill_rect(Random.int(mymatrix_dx()), Random.int(mymatrix_dx()))
  )
  else
    (
      (set_color(randomcolor()));
      myfill_rect(Random.int(mymatrix_dx()), Random.int(mymatrix_dx()));
      
      endgame(x - 1);
    )
;;

let rec randombonus(x : int) : unit =

  if x = 0
  then
  (set_color(blue);
   myplot(Random.int(mymatrix_dx()), Random.int(mymatrix_dx()))
  )
  else
    (
      (set_color(blue));
      myplot(Random.int(mymatrix_dx()), Random.int(mymatrix_dx()));
      
      randombonus(x - 1);
    )
;;

randombonus(50000);;


let waitf (sec: float)  =
    (Unix.select [] [] [] sec);;

set_line_width(20);;

let pi = 3.14159265359;;
let increase = pi /. 100.;;


let mathgraph(): unit =
  let x : float ref = ref 0. in
  let y : float ref = ref 0. in
  let counter : float ref = ref 0. in
  for i = 0 to 10000
  do
    (counter := !counter +. increase);
    (x := float_of_int(i) /. 10.);
    (y := sin(!counter));
    (fill_rect(int_of_float(!x)*5, int_of_float(!y)*5,1,1))
  done;
    
    ;;

let draw_string_v(s : string) : unit =
 let(xi,yi) = Graphics.current_point()
   and l = String.length s
   and(_,h) = Graphics.text_size s
   in
   Graphics.draw_char s.[0];
   for i=1 to l-1 do
     let(_,b) = Graphics.current_point()
     in
     Graphics.moveto xi(b-h);
     Graphics.draw_char s.[i]
   done;
   let(a,_) = Graphics.current_point() in Graphics.moveto a yi;;

draw_string_v("dfsdfdsfdsf");;
