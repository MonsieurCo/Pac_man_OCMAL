(*#load "graphics.cma";;*)

open Graphics;;
open_graph " 1920x1080";;

module type UF = sig
  type uf
  val makeset: int -> uf
  val find: uf -> int -> int
  val union: uf -> int ->int -> unit
end

module Uf : UF = struct
  type uf = {
    mutable rang:int array;
    mutable parent:int array}

  let makeset x =
    let uf = {rang = (Array.make x 0); parent = (Array.init x (fun i -> i))} in
    uf ;;

  let rec find uf i =
    let pi = uf.parent.(i) in
    if pi == i then
      i
    else begin
      let racine = find uf pi in
      uf.parent.(i) <- racine; (* path compression *)
      racine
    end

  let union t x y =
    let xr = find t x in
    let yr = find t y in
    if xr <> yr then
      if t.rang.(xr) < t.rang.(yr) then
        t.parent.(xr) <- yr
      else
        t.parent.(yr) <- xr;
    if t.rang.(xr) = t.rang.(yr) then
      t.rang.(xr) <- t.rang.(xr) +1;;
end

open Uf;;

let lost = ref false;;
let win = ref false;;

let case_pacman = ref 0;;
let case_fantome = ref 1;;

(* LABYRINTHE *)
let mur_au_hasard l h =
  let x = Random.int h in
  let y = Random.int l in
  let d = Random.int 2 in
  if x = (h-1) then
    if y = l-1 then
      (0,x,y-1)
    else (0,x,y) else
  if y = (l- 1) then
    if x < h-1 then
      (1,x,y)
    else (1,x-1,y) else
    (d , x , y);;

let cases_adjacentes l h (d,x,y) = match d with
  | 1 -> ((x*l)+y, (x*l)+y + l)
  | 0 -> ((x*l)+y,(x*l)+y+1)
  | _ -> invalid_arg "not a direction";;

let mur_presentgen l h =
  let mur =  Array.make 2 [||]  in
  for i = 0 to 1 do
    mur.(i) <- Array.make h [||];
    for  j = 0 to h-1 do
      mur.(i).(j) <- Array.make l true ;
    done;
  done;
  mur;;

let generate_lab_rec l h =
  let mur_present = mur_presentgen l h in
  let uf = makeset (l*h) in
  let a = ref 1 in
  let rec aux e b =
    if !a = b then print_int(1) else
      let (d, x, y) = mur_au_hasard l h in
      let (i, j) = cases_adjacentes l h (d, x, y) in
      if find uf i <> find uf j
      then
        begin
          union uf i j;
          mur_present.(d).(x).(y) <- false;
          a := !a +1 ;
          aux !a b;
        end
      else
        aux (!a) b;
  in aux !a ((l*h)) ;
  mur_present;;

(* GRAPHISMES *)
set_line_width 4;;
let trace_pourtour upleftx uplefty taille_case l h =
  moveto upleftx uplefty;
  lineto ( current_x() + (taille_case * l)) (current_y()) ;
  lineto (current_x()) ((current_y()) - (taille_case * (h-1)));
  moveto (current_x()) ((current_y())-taille_case);
  lineto ((current_x()) - (taille_case * l)) (current_y());
  lineto (upleftx) (uplefty- taille_case);;

let trace_mur upleftx uplefty taille_case (d,x,y) =
  moveto upleftx uplefty ;
  let (a,b) = (((current_x())+(y* taille_case)) , ((current_y())-(x* taille_case)))  in
  moveto (a+taille_case) b ;
  if d = 0
  then lineto (current_x()) ((current_y())-taille_case)
  else begin
    moveto a (b-taille_case) ;
    lineto ((current_x())+ taille_case ) (current_y()) end ;;

let trace_lab upleftx uplefty taille_case l h mur_present=
  trace_pourtour upleftx uplefty taille_case l h;
  for d = 0 to 1 do
    for x = 0 to (h-1) do
      for y = 0 to (l-1) do
        if y = (l-1) && d = 0 then () else begin
          if mur_present.(d).(x).(y) = true then
            trace_mur upleftx uplefty taille_case (d, x, y)
        end
      done
    done
  done;;

let dessine_pac(case_pacman,c,tc) = begin
  let x = fst case_pacman in
  let y = snd case_pacman in
  let taille = abs(tc/2 - 5) in
  let oeil(x,y) =
    set_color black;
    fill_circle (x + 5) (y+7) 3 in
  let bouche(x,y) =
    set_color white;
    fill_arc  x y  taille  taille (-taille) 10  in
  set_color c;
  fill_circle x y taille;
  oeil(x,y);
  bouche(x,y);
  set_color black;
end;;

(* PAC-MAN *)
let move_pacman l h lab = 
  let key = read_key() in
  let yp =  ref (!case_pacman mod l )in
  let xp = ref ( !case_pacman /  l) in
  match key with
  | 'z' -> if (!case_pacman >= l ) && (not lab.(1).(!xp-1).(!yp))
    then case_pacman := !case_pacman-l
    else begin case_pacman := !case_pacman ; sound 15000 100; end
  | 's' -> if not lab.(1).(!xp).(!yp)
    then case_pacman := !case_pacman+l
    else  begin case_pacman := !case_pacman ; sound 15000 100; end
  | 'q' ->  if (!yp >= 1 ) && (not lab.(0).(!xp).(!yp-1))
    then case_pacman := !case_pacman-1
    else  begin case_pacman := !case_pacman ; sound 15000 100; end
  | 'd' ->  if (!case_pacman < (l*h)-1 ) && (not lab.(0).(!xp).(!yp))
    then case_pacman := !case_pacman+1
    else
    if !case_pacman = (l*h)-1
    then win := true
    else  begin case_pacman :=  !case_pacman ; sound 15000 100; end
  | _ -> invalid_arg"wola";;

(* Fantome *)
let movefantome case_pac case_fantome l h = 
  let yf =  (case_fantome mod l )in
  let xf = ( case_fantome /  l) in
  let yp =   (case_pac mod l )in
  let xp = ( case_pac /  l) in 
  match xf, yf , xp, yp with
  | xf , yf , xp , yp when yf = yp ->  if xp > xf then case_fantome + l else case_fantome -l
  | xf , yf , xp , yp when xf = xp  -> if yf > yp then  case_fantome - 1 else case_fantome + 1
  | xf , yf , xp , yp when yf < yp && xf < xp  -> case_fantome + l
  | xf , yf , xp , yp when yf < yp && xf > xp  -> case_fantome - l 
  | xf , yf , xp , yp when yf > yp && xf < xp  -> case_fantome + l
  | xf , yf , xp , yp when yf > yp && xf > xp  -> case_fantome - 1
  | xf , yf , xp , yp -> case_fantome;;

(* Positions *)
let generate_pos upleftx uplefty taille_case l h =
  let pos = Array.init (l*h) (fun i -> (((i+1)*upleftx))+(taille_case / 2),((i+1)*uplefty)-taille_case/2) in
  pos;;

let gen_pos upleftx uplefty taille_case l h =
  let pos = Array.init (l*h) (fun i -> (0,0)) in

  for i = 0 to (l-1) do
    if i = 0 then  pos.(0) <- ((upleftx + (taille_case/2)), (uplefty - (taille_case/2)) )
    else begin
      pos.(i)  <- ( fst (pos.(i-1))+taille_case, snd (pos.(i-1)));
    end;
    for j = 1 to (h-1) do
      pos.(i+(j*l)) <- (fst(pos.(i+(j*l)-l)),snd (pos.(i+(j*l)-l))-taille_case);
    done
  done;
  pos;;

let center (p:(int *int )array ) =
  set_color black;
  for i= 0 to (Array.length p) - 1 do
    let a = fst p.(i)in
    let b = snd p.(i) in
    fill_circle a b  3;
  done;;

(* GAME *)
let check_loose () =
  if !case_fantome = !case_pacman then 
    begin
      lost := true;
      clear_graph ();
      moveto 500 500;
      set_text_size 20;
      draw_string "Perdu"; 
    end;;

let threaded_ghost (upleftx, uplefty, taille_case, case_pac, case_fantome, l, h, lab, pos) = 
  dessine_pac(pos.(!case_fantome),red,taille_case) ;
  while not !win && not !lost do
    Unix.sleep 2;
    if not !win && not !lost
    then begin
     
      check_loose ();
      if not !lost then begin
        clear_graph ();
        (*center pos;*)
        dessine_pac(pos.(!case_pacman),yellow,taille_case) ;
        case_fantome := movefantome !case_pacman !case_fantome l h ;
        dessine_pac(pos.(!case_fantome),red,taille_case) ;
        trace_lab upleftx uplefty taille_case l h (lab);
      end;
    end;
    check_loose ();
    if !win then
      begin
        clear_graph ();
        moveto 500 500;
        set_text_size 20;
        draw_string "Gagne";
      end;
  done;  
;;

let game upleftx uplefty taille_case l h = 
  let r = Random.self_init () in
  case_fantome := l-1 ;
  let lab =  generate_lab_rec l h  in
  let pos = gen_pos upleftx uplefty taille_case l h in
  let _ = Thread.create threaded_ghost (upleftx, uplefty, taille_case, case_pacman, case_fantome, l, h, lab, pos) in
  clear_graph ();
  trace_lab upleftx uplefty  taille_case l h (lab);
  (*center pos;*)
  dessine_pac(pos.(!case_pacman),yellow,taille_case);
  while not !win && not !lost do
    move_pacman l h lab;
    check_loose ();
    if not !lost then begin
      clear_graph ();
      (*center pos;*)
      dessine_pac(pos.(!case_pacman),yellow,taille_case) ;
      (*case_fantome := movefantome !case_pacman !case_fantome l h ;*)
      dessine_pac(pos.(!case_fantome),red,taille_case) ;
      trace_lab upleftx uplefty taille_case l h (lab);
    end;
    check_loose ();
    if !win then
      begin
        clear_graph ();
        moveto 500 500;
        set_text_size 20;
        draw_string "Gagne";
      end;
  done;
;;

let () = 
  game 100 625 40 29 15;
;;

ignore  @@ Graphics.read_key();;