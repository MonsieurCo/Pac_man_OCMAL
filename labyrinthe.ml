open Format
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



let mur_au_hasard l h = (* renvoie un triplet (d, x, y) *) 
    let n = Random.int ((l*h)-2) in
    if n > l * (h-1)
        then (0, n mod (l-1), n / (l-1))
    else 
        let bin = Random.int 1 in 
        if ((n mod l) = (l-1) ) then (1, (n mod l), (n / l) )
        else (bin, n mod l, n / l);;


let cases_adjacentes l h (d,x,y) = match d with
    | 1 -> ((x*l)+y, (x*l)+y + l)
    | 0 -> ((x*l)+y,(x*l)+y+1)
    | _ -> invalid_arg "not a direction"
    
    
(*
    cases_adjacentes 5 5 (0,0,2)  
    cases_adjacentes 3 3 (1,2,0)  
    cases_adjacentes 5 5 (1,0,1) *)
    
let generate_lab l h = 
    let mur_present = Array.make 2 (Array.make l (Array.init h (fun x -> true))) in
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
    
        
    in aux !a ((l-1)*h + l*(h-1)) ;
   mur_present;;

let generate_lab2 l h =
  let mur_present = Array.make 2 (Array.make l (Array.init h (fun i -> true))) in
 let uf = init (l*h) in
  let acc = ref 1 in
  while !acc < (l * h) - 1 do
    let (d, x, y) = mur_au_hasard_improved l h in
    let (i, j) = cases_adjacentes l h (d, x, y) in
    (*if i < (l * h) - 1 && j < (l * h) - 1 then*)
     begin
      printf "%d %d %d %d\n" x y ((l * h) - 1) !acc;
      if find uf i <> find uf j
      then begin
        union uf i j;
        mur_present.(d).(x).(y) <- false;
        acc := !acc + 1;
      end
    end
    (* else *)
  done;
  (mur_present, uf);;


let test() = 
        let m = mur_au_hasard 5 5 in 
        let (i,j) = cases_adjacentes 5 5 m in 
        (i,j);;