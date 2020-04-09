#use "CPutil_sn.ml";;
#use "CPinter_sn.ml";;

(* Binome:
Radu GHEORGHIU
Adam SAYED ABOULJOUD*)

(*PARTIE TYPE/VARIABLES*)

type t_direction = UP | RIGHT | DOWN | LEFT ;;

type t_point = {x : int ; y : int} ;;

type t_position = {pt : t_point ; dir : t_direction} ;;
(*La position d'un element du jeu est decris par une coordonee x, y et sa direction dir*)

type t_value = EMPTY | SNAKE | FRAME | PROBLEM | BONUS ;;
(*Le type t_value est essentiel pour decrire les elements du jeu*)

type t_matrix = t_value matrix;;
(*Le plateau de jeu est une matrice t_value car chaque element dans la matrice auras un type t_value *)

type t_snake = t_position list ;;
(*Sachant que le serpent est costruit a partir de elements de base de la matrice, on as besoin d'un liste pour contenir tous ses elements *)

type t_play = {dt : float ref ; sn : t_snake ref ; mat : t_matrix};;
(* dt = vitesse du serpent *)

let mymatrix_dx() : int = 100;;
(*taille de la matrice en x*)
let mymatrix_dy() : int = 100;;
(*taille de la matrice en y, matrice qui va nous servir comme plateau de jeu*)

let mydt_init() : float = 0.5;;
(*Vitesse initiale du serpent*)

let mydt_acc() : float = 0.5;;
(*Acceleration du serpent*)

let mydt_ratio() : float = 0.9;;

let mytranslation_x() : int = 100;; 
let mytranslation_y() : int = 100;;
(*On veut que l'affichage correspond a la matrice*)

let mydilation_x() : int = 5;;
let mydilation_y() : int = 5;;
(*On veut que chaque element ou "pixel" sur la graphique correspond a 5 pixels reels*)

let mysnake_length_init() : int = 5;;
(*Initialisation de la taille du serpent*)

let mysnake_position_init(): t_point = { x = mymatrix_dx()/2 ; y = mymatrix_dy()/2};;
(*On veut que la position initiale du snake soit au milieu de la matrice*)




(*PARTIE GRAPHIQUE*)

let mygraphic_x(x : int) : int =
  x * mydilation_x() + mytranslation_x()
;;
(*calcul des coordonees graphiques en x en fonction des coordonees de la matrice,
ou on multiplie la coordonee x par le coeff d'homothetie et on y rajoute le coeff de
translation*)
let mygraphic_y(y : int) : int =
  y * mydilation_y() + mytranslation_y()
;;
(*calcul des coordonees graphiques en y en fonction des coordonees de la matrice*)

let myplot(x, y : int * int) : unit =
  fill_rect(mygraphic_x(x), mygraphic_y(y),
            mydilation_x(), mydilation_y())
;;
(*Fonction qui trace un rectangle correspondant a un element/point dans la matrice,
ou on utilise mygraphic pour la coordonee, et mydilation pour la taille du rectangle *)

let myfill_rect(px, py, dx, dy : int * int * int * int) : unit =
      fill_rect(mygraphic_x(px), mygraphic_y(py), mydilation_x()*dx,mydilation_y()*dy);
;;
(*Fonction qui trace un rectangle qui correspond au coordonees px, py et la taille dx et dy *)
                     
let color_of_value(x : t_value) : t_color =
  if x = EMPTY
  then
    white
  else
    if
      x = SNAKE
    then
      black
    else
      if
        x = FRAME
      then
        green
      else
        if x = PROBLEM
        then
          red
        else
          blue
;;
(*Fonction qui definie les couleurs des elements t_value*)

let draw_frame(): unit =
  set_color(color_of_value(FRAME));
  myfill_rect(-1, mymatrix_dy(), mymatrix_dx() + 2, 1);
  myfill_rect(-1, -1, mymatrix_dx() + 2, 1);
  myfill_rect(-1, -1, 1, mymatrix_dy() + 2);
  myfill_rect(mymatrix_dx(), -1, 1, mymatrix_dy() + 2)
;;
(*Fonction qui dessine le frame a l'aide de myfill_rect,
ex: myfill_rect( (coordonee en x),(coordonee en y) , (longueur en x),(longueur en y)  *)

let rec draw_whole_snake(s : t_snake) : unit =
 if len(s) = 0
 then
    ()
 else
 (
    set_color(color_of_value(SNAKE));
    myplot((fst(s)).pt.x,(fst(s)).pt.y);
    draw_whole_snake(rem_fst(s))
 )
;;
(*Ici on dessine sur le graphique tous les elements de s, qui est un type t_snake donc un liste de t_position,
la condition d'arret de la fonction recursive est que la longueur de la liste s soit nulle.
dans le else, on utilise d'abords color_of_value pour trouver la couleur de l'element SNAKE 
et puis on lui raccorde avec set_color, ensuite on dessine sur le graphique le point qui correspond aux 
coordonees du premier element de la liste s, et ensuite on fait l'appel recursif avec le premier element 
de la liste enlevé*)



(*PARTIE LOGIQUE*)
let rec aux_init_snake (s : t_snake) : t_position list =
  let sneklen : int =  len(s) in
  if
    sneklen < (mysnake_length_init())
  then
        aux_init_snake(add_fst(s,{pt =
                        { x = (((mysnake_position_init()).x) + sneklen);
                          y = ((mysnake_position_init()).y)};
                               dir = RIGHT}))
  else
        s
;;
(*Ici on initialise le serpent, on part d'une liste vide s, et on cree un serpent de longueur mysnake_length_init() 
et a la position mysnake_position_init, la condition d'arret de l'appel recursif est que la longueur de la liste s soit egale a la 
variable d'initialisaition mysnake_length_init, dans le else on fait simplement lappel recursif en y rajoutant a chaque fois 
un element dans la liste s dans la direction RIGHT*)

let init_snake() : t_snake =
  let snake : t_snake = [] in
  aux_init_snake(snake)
;;
(*On initialise le serpent en utilisant la fonction recursive aux_init_snake, le seul but de cette fonction est d'accorder une liste
vide a notre snake, pour que la fonction aux_init_snake peut s'executer *)

let init_matrix() : t_matrix =
  let matrix = mat_make(mymatrix_dx(), mymatrix_dy(), EMPTY : int * int * t_value) in
(
for i = 0 to mymatrix_dx()-1
do
      (matrix.(i).(0) <- FRAME);
      (matrix.(0).(i) <- FRAME);
      (matrix.(i).(mymatrix_dx()-1) <- FRAME);
      (matrix.(mymatrix_dy()-1).(i) <- FRAME);
done;
);
matrix
;;
(*On ititialise la matrice de de base du jeu, avec chaque case de la matrice qui as un t_value EMPTY, et ensuite on 
y rajoute le t_value FRAME, qui correspond au frame que on as dessiner avec la fonction draw_frame *)

let init_snake_matrix() : t_snake * t_matrix = 
  let matrix = init_matrix()
  and snake = init_snake() in
  for i = 1 to len(snake)
  do 
    (matrix.((nth((snake),i-1)).pt.x).((nth((snake), i-1)).pt.y) <- SNAKE);
  done;
  (snake, matrix); 
;;
(*Ici on initialise la matrice avec init_matrix, qui contient le FRAME et le plateau de jeu, et on y rajoute a la matrice
les t_value SNAKE qui correspondent a notre liste t_position apres l'initialisation du serpent avec init_snake,
on utilise la boucle qui fonctionne pour chaque element dans la liste, et on rajoute le t_value SNAKE aux coordonees des elements
de la liste snake *)

let init_play() : t_play =
  let dt : float = mydt_init() and (sn, mat) : t_snake * t_matrix = init_snake_matrix() in
  draw_frame();
  draw_whole_snake(sn);
  {dt = ref dt; sn = ref sn; mat = mat}
;;
(*On initialise le jeu, on y dessinant le frame et le snake, et on initialise la matrice avec tous les elements dedans,
le resultat de cette matrice est un type t_play *)

let compute_new_position(pos, d : t_position * t_direction) : t_position =
  if d = RIGHT
  then
    ({pt = { x = pos.pt.x +1;
            y = pos.pt.y};
            dir = d})
  else
    if
      d = LEFT
    then
      ({pt = { x = pos.pt.x - 1;
               y = pos.pt.y};
               dir = d})
    else
      if
        d = UP
      then
        ({pt = { x = pos.pt.x;
                 y = pos.pt.y+1};
                 dir = d})
      else
          ({pt = { x = pos.pt.x;
                   y = pos.pt.y-1};
                   dir = d})  
;;
(*Fonction assez simple meme si complique visuellement, elle nous sert a calculer une nouvelle t_position a partir
d'une t_position de base et une nouvelle direction
ex: compute_nex_position((x = 55,y = 55),RIGHT) va nous donner (x = 56, y = 55)  *)

let compute_move(pos, dir, m : t_position * t_direction * t_matrix) : t_position * t_value =
  let new_pos : t_position = compute_new_position(pos, dir) in
  if
    new_pos.pt.x < 0 || new_pos.pt.x > (mymatrix_dx()-1) || new_pos.pt.y < 0 || new_pos.pt.y >  (mymatrix_dx()-1)
  then
    (new_pos, FRAME)
  else
    (new_pos, m.(new_pos.pt.x).(new_pos.pt.y))
;;
(*Fonction qui calcule sur quel element dans la matrice on va atterrir en fonction de notre position et notre direction de mouvement,
la condition du if etant pour savoir si on sort du frame ou pas, si notre position sera egale aux coordonees du frame, la fonction
nous donne la nouvelle position et le t_value FRAME, sinon dans le else on nous donne la nouvelle position et le t_value associee *)
 
let remove_snake_tail(pl : t_play) : unit =
  set_color(color_of_value(EMPTY));
  myplot((lst(!(pl.sn))).pt.x, (lst(!(pl.sn))).pt.y);
  pl.mat.((lst(!(pl.sn))).pt.x).((lst(!(pl.sn))).pt.y) <- EMPTY;
  pl.sn := rem_lst(!(pl.sn));
;;
(*Avec cette fonction, on enleve un element de la matrice et on efface son dessin associe sur le graphique qui correspond au 
tail du serpent, pour effacer on utilise un myplot avec les coordonees du dernier element de la liste snake et un set_color EMPTY
ensuite avec les memes coordonees on change le t_value dans la matrice avec EMPTY *)

let add_snake_newhead(pl, newpos : t_play * t_position) : unit =
  pl.sn := add_fst(!(pl.sn), newpos);
  pl.mat.(newpos.pt.x).(newpos.pt.y) <- SNAKE;
  set_color(color_of_value(SNAKE));
  myplot(newpos.pt.x, newpos.pt.y);
;;
(*Pareil que pour remove_snake_tail sauf inverse*)

let move_snake(pl, newpos : t_play * t_position) : unit =
  remove_snake_tail(pl);
  add_snake_newhead(pl, newpos);
;;
(*On combine les deux fonctions precedentes pour donner l'illusion que le serpent s'est deplace d'une case *)

let analyze(pos : t_position) : t_direction =
  let (i, j) : int * int = mouse_pos() in
  let (k, l) : int * int = (mygraphic_x(pos.pt.x), mygraphic_y(pos.pt.y)) in
  let v : t_direction =
    if abs(i - k) < abs(j - l)
    then 
      if j > l 
      then UP
      else DOWN
    else
      if i < k
      then LEFT
      else RIGHT
  in
    if (pos.dir = UP && v = DOWN) || (pos.dir = DOWN && v = UP)
    then if i < k then LEFT else RIGHT
    else 
      if (pos.dir = LEFT && v = RIGHT) || (pos.dir = RIGHT && v = LEFT)
      then if j > l then UP else DOWN
      else v
 ;;

let new_step(pl : t_play) : bool =
  let (newpos, value) : t_position * t_value = compute_move(fst(!(pl.sn)), analyze(fst(!(pl.sn))), pl.mat) in
  move_snake(pl, newpos);
  if (value = EMPTY)
  then false
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
(*Ici on utilise les fonctions precedentes pour deplacer le snake en fonction de la position de la souris,
  le resultat est un bool, false si la case de la matrice sur laquelle le serpent atterri est EMPTY et true si c'est FRAME ou SNAKE,
si c'est FRAME ou SNAKE, le jeu s'arrete, on dessine en couleurs de PROBLEM l'endroit du choque et on dessine GAME OVER*)

let handle_t_acc(t, t_acc, play : float ref * float ref * t_play) : unit = 
  if (!t -. !t_acc) > mydt_acc()
  then 
    (
    play.dt := !(play.dt) *. mydt_ratio() ; 
    t_acc := !t ;
    ) 
;;
(*Cette fonction utilise les variables qu'on as declarer au debut, mydt_acc et mydt_ratio pour calculer la vitesse d'acceleration en fonction de t et t_acc, mydt_ratio decrit la frequence de l'acceleration et mydt_acc l'acceleration*)

let simulation() : unit =
  let pl : t_play = init_play() in
  let t : float ref = ref (Sys.time()) and newt : float ref = ref (Sys.time())  in
  let tacc : float ref = ref (Sys.time()) in 
  let thend : bool ref = ref false in
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
    done ;
    )
;;
(*La fonction simulation utilise handle_t_acc pour calculer l'acceleration du serpent en fonction du temps, ce qui nous permet d'accelerer le deplacement du serpent au cours du temps, on prend le temps initiale avec sys.time et plus le temps s'ecoule, le plus que le serpent accelere *)
