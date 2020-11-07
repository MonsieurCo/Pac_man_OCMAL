
#load "graphics.cma";; 

open Graphics;;
open_graph " 1920x1080";; 






let init n =
	Array.init n (fun i -> i);;


let rec find t x =
    if t.(x) = x then x 
    else begin
        t.(x) <- find t t.(x);
    t.(x) end
    ;;
let union t x y =
    let xr = find t x in
    let yr = find t y in
    if xr <> yr then
        t.(xr) <- yr


let mur_au_hasard_improved l h = 
    let x = Random.int h in 
    let y = Random.int l in
    let d = Random.int 2 in  
    if x = (l-1) then 
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
    | _ -> invalid_arg "not a direction"
    
    
let check t =  
let k =  ref false in
    for i = 0 to (Array.length t)-2 do
        if t.(i) <> t.(i+1) then k := false else k:=true 
       
         done;
         !k;
         ;; 


let mur_presentgen l h = 
    let mur =  Array.make 2 [||]  in 
    for i = 0 to 1 do
        mur.(i) <- Array.make 5 [||];
        for  j = 0 to h-1 do 
            mur.(i).(j) <- Array.make l true ;

        done;
    done;
    mur;;



let generate_lab2 l h =
  let mur_present = mur_presentgen l h in
  let uf = init (l*h) in
  let acc = ref 1 in
    while  !acc < (l*h)-1   do
    
    let (d, x, y) = mur_au_hasard_improved l h in
    let (i, j) = cases_adjacentes l h (d, x, y) in
    (*if i < (l * h) - 1 && j < (l * h) - 1 then*)
     begin
      
      if find uf i <> find uf j
      then begin
        union uf i j;
        
        mur_present.(d).(x).(y) <- false;
        acc := !acc + 1;
      end
    end
    (* else *)
  done;
  mur_present;;

(*generate_lab2 5 5 ;;
*)

set_line_width 4;;
let trace_pourtour upleftx uplefty taille_case l h =
  moveto upleftx uplefty;
  lineto ( current_x() + (taille_case * h)) (current_y()) ;
  lineto (current_x()) ((current_y()) - (taille_case * l));
  lineto ((current_x()) - (taille_case * h)) (current_y());
  lineto upleftx uplefty;;


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
        for x = 0 to (l-1) do 
            for y = 0 to (h-1) do
                if mur_present.(d).(x).(y) = true then
                    trace_mur upleftx uplefty taille_case (d, x, y)
                    
            done 
        done 
    done;;

trace_lab 100 500 50 5 5 (generate_lab2 5 5);


ignore  @@ Graphics.read_key();;
