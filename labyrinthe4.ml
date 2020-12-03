(*#load "graphics.cma";;*)

open Graphics;;
open_graph " 800x800";;

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
    if uf.parent.(i) == i then
      i
    else begin
      uf.parent.(i) <- find uf uf.parent.(i);
      uf.parent.(i)
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

(*Declaration des références globales *)
let lost = ref false;;
let win = ref false;;

let case_pacman = ref 0;;
let case_fantome = ref 1;;

let voisine_relie = ref false;;

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


(*    dessine path finding *)

let draw_path case_pacman = begin
  let x = fst case_pacman in
  let y = snd case_pacman in
  let color = green in
  set_color color;
  fill_circle x y 10 ;
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
  | 'a' -> invalid_arg"game exited...";
  | _ ->  sound 15000 100;; 

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



(*Renvoie un tableau de 4 valeurs corspondantes aux cases voisines de la case i , 
si la case n'existe pas exemple on demande la voisine gauche quand on ets au 
bord du plateau on retourne la case courante  
 les cases sont rendu dans l'ordre gauche , haut , droit , bas *)
let case_voisines i l h = Printf.printf " <case voisines> appélé pour %d\n " i;
                          if i < l && i > 0 && i <(l-1) then [|i-1;i;i+1;i+l|] 
                          else if i < l && i > 0 && i = (l-1) then [|i-1;i;i;i+l|]  
                          else if i mod l = 0 && (i/l) = 0 then [|i;i;i+1;i+l|] 
                          else if (i mod l ) = 0 && (i / l)  < h-1 then [|i;i-l;i+1;i+l|]  
                          else if (i mod l ) = 0 && (i / l) = h-1 then [|i;i-l;i+1;i|] 
                          else if (i mod l ) = (h-1) && (i / l)  < h-1 then [|i-1;i-l;i;i+l|] 
                          else if (i mod l ) = (h-1) && (i / l) = h-1 then [|i-1;i-l;i;i|] 
                          else if (i/l) = h-1 then [|i-1;i-l;i+1;i|] 
                          else [|i-1;i-l;i+1;i+l|] ;;

(*Retourne un tableau de longueur varable de cases inaccessible depuis la case i *)
let evite i l h mur_present = 
                  let voisin = case_voisines i l h in
                
                  let give_x case = case / l in 
                  let give_y case = case mod l in 
                  let tab = ref [||] in 
                  for j=0 to 3 do 
                
                  if voisin.(j) <> i then
                  let x = give_x voisin.(j) in 
                  let y = give_y voisin.(j) in 

                  if voisin.(j) = i-1 then begin 
                  if (mur_present.(0).(x).(y) = true) then tab := Array.append !tab [|voisin.(j)|] end; 
                  if voisin.(j) = i-l then begin 
                  if (mur_present.(1).(x).(y) = true) then tab := Array.append !tab [|voisin.(j)|] end;
                  if voisin.(j) = i+1 then begin 
                      let ix = give_x i in 
                      let iy = give_y i in 
                  if (mur_present.(0).(ix).(iy) = true) then tab := Array.append !tab [|voisin.(j)|] end;
                  if voisin.(j) = i+l then 
                  begin 
                      let ix = give_x i in 
                      let iy = give_y i in 
                  if (mur_present.(1).(ix).(iy) = true) then tab := Array.append !tab [|voisin.(j)|] end;
                  done;
                  !tab;;

(*A partir d'un tableau de cases a éviter et d'un tableau de cases voisines retorune un tableau de cases accessibles mais pas forcément relié au pacman *)
let disponible evite voisine src = 
              let tab = ref [||] in
              for i=0 to (Array.length voisine) -1 do 
                if (not (Array.memq voisine.(i) evite )) && voisine.(i) <> src then 
                  tab := Array.append !tab [|voisine.(i)|];
                done; 
    
              !tab;;    







let rec est_relie src dst evite voisines pos =
  (*l'argument pos ici est acutellement inutile mais sert a l'affichage de la recherche de chemin pour le debug *)
  (*draw_path pos.(src);
  ignore (Unix.select [] [] [] 0.1);*)
  if src = dst then begin
    voisine_relie := true;
    true
  end
  else
    begin
      for c = 0 to Array.length voisines.(src) - 1 do
        (*Printf.printf "%d\n" voisines.(src).(c);*)
        if evite <> voisines.(src).(c) && voisines.(src).(c) <> src then begin
          if est_relie voisines.(src).(c) dst src voisines pos
            then true
          else false;
        end
        else false
      done;
      false
    end;;




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

let break = ref false;;


let threaded_ghost (upleftx, uplefty, taille_case, case_pac, case_fantome, l, h, lab, pos,mur_present) = 
  dessine_pac(pos.(!case_fantome),red,taille_case) ;
  let voisine = ref (Array.init (l*h) (fun i -> case_voisines i l h )) in
  let evite_array = ref (Array.init (l*h) (fun i -> evite i l h mur_present )) in
  let dispo = Array.init (l*h) (fun i-> disponible !evite_array.(i) !voisine.(i) i)in
  while not !win && not !lost do
    Unix.sleep 2;
   (* ignore (Unix.select [] [] [] 0.1);*)
    if not !win && not !lost
    then begin 
      
      (*let chem = chemin !case_fantome !case_pac mur_present l h pos in*)

      check_loose ();
      if not !lost then begin
        clear_graph ();
        (*center pos;*)
        dessine_pac(pos.(!case_pacman),yellow,taille_case) ;
      
          for i = 0 to (Array.length dispo.(!case_fantome)) - 1 do
            (*Printf.printf "%d\n" dispo.(!case_fantome).(i);*)
            if not !voisine_relie then begin 
            ignore @@ est_relie dispo.(!case_fantome).(i) !case_pacman !case_fantome dispo pos end;
            if not !break && !voisine_relie  then begin
              case_fantome := dispo.(!case_fantome).(i);
              break := true
            end
          done;
          voisine_relie := false;
          break := false;
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
  let _ = Thread.create threaded_ghost (upleftx, uplefty, taille_case, case_pacman, case_fantome, l, h, lab, pos,lab) in
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
  game 100 625 40 10 10 ;
;;

ignore  @@ Graphics.read_key();;