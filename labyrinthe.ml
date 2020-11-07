
#load "graphics.cma";; 

open Graphics;;
open_graph " 1920x1080";;
(*UF *)
let max_rang = ref 0;;
type partition = (int * int) array;;


let init n =
	Array.init n (fun i -> (i, 0));;




let rec find t i =
	if fst t.(i) = i then
		i
	else begin
		let aux = find t (fst t.(i)) in
		t.(i) <- (aux, snd t.(i)); 
		aux 
	end;;


let union t x y =
	let i = find t x in
	let j = find t y in
	let rec aux t i j =
		(* Union par rang. *)
		if snd t.(i) <= snd t.(j) then begin
			t.(i) <- (j, snd t.(i));
			if snd t.(i) = snd t.(j) then begin
				t.(j) <- (fst t.(j), snd t.(j) + 1);
				max_rang := max !max_rang (snd t.(j))
			end
		end
		else begin
			aux t j i
		end
	in if i <> j then aux t i j;;








(* fin UF *)

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

mur_au_hasard_improved 5 5 ;;


let mur_au_hasard l h = (* renvoie un triplet (d, x, y) *) let n = Random.int ((l-1) * h + l * (h-1)) in
if n < (l-1) * h
then (0, n mod (l-1), n / (l-1))
else let n2 = n - (l-1) * h in (1, n2 mod l, n2 / l)

let cases_adjacentes l h (d,x,y) = match d with
    | 1 -> ((x*l)+y, (x*l)+y + l)
    | 0 -> ((x*l)+y,(x*l)+y+1)
    | _ -> invalid_arg "not a direction"
    
    
(*
    cases_adjacentes 5 5 (0,0,2)  
    cases_adjacentes 3 3 (1,2,0)  
    cases_adjacentes 5 5 (1,0,1) *)


    let mur_presentgen l h = 
    let mur =  Array.make 2 [||]  in 
    for i = 0 to 1 do
        mur.(i) <- Array.make 5 [||];
        for  j = 0 to h-1 do 
            mur.(i).(j) <- Array.make l true ;

        done;
    done;
    mur;;



    
let generate_lab l h = 
    let mur_present = mur_presentgen l h in
    let uf = init (l*h) in
    let a = ref 1 in
    let rec aux e b = 
        if !a = b then print_int(1) else
        let (d, x, y) = mur_au_hasard_improved l h in
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
        print_int(12);
    
        
    in aux !a ((l*h)) ;
   mur_present;;

(*0 verticale 1 horizontal *)

let generate_lab_taki l h =
  let mur_present = mur_presentgen l h in
  let uf = init (l*h) in
  let acc = ref 1 in
  while !acc < (l * h) do
    let (d, x, y) = mur_au_hasard_improved l h in
    let (i, j) = cases_adjacentes l h (d, x, y) in
      if find uf i <> find uf j
      then begin
        union uf i j;
        Printf.printf "d = %d x = %d y = %d\n" d x y;
        mur_present.(d).(x).(y) <- false;
        acc := !acc + 1;
      end
    
  done;
  mur_present

let generate_lab2 l h =
    let mur_present = mur_presentgen l h in
    let uf = init (l*h) in
    let acc = ref 1 in
    while !acc < (l * h) do
    
        let (d, x, y) = mur_au_hasard_improved l h in
        let (i, j) = cases_adjacentes l h (d, x, y) in
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





trace_lab 100 500 50 5 5 (generate_lab_taki 5 5);

(*
trace_lab 0 800 50 100 500 (generate_lab2 100 500);
trace_pourtour 600 600 50 5 5;;
trace_mur 600 600 50 (0,0,2);;
trace_mur 600 600 50 (1,0,0);;
trace_mur 600 600 50 (1,0,1);;
trace_mur 600 600 50 (1,2,1);;
trace_mur 600 600 50 (0,1,1);;
trace_mur 600 600 50 (1,0,3);;
trace_mur 600 600 50 (1,1,0);;
trace_mur 600 600 50 (0,2,0);;
trace_mur 600 600 50 (0,1,3);;
trace_mur 600 600 50 (0,3,2);;
trace_mur 600 600 50 (0,3,3);;
trace_mur 600 600 50 (0,4,2);;
trace_mur 600 600 50 (1,3,4);;
trace_mur 600 600 50 (1,1,2);;
*)
ignore  @@ Graphics.read_key();;


(*let test() = *)
  