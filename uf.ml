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

