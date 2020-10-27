#load "graphics.cma";; 
open Graphics;;
open_graph " 1920x1080";;
Graphics.open_graph " 1920x1080";;
let x = 500;;
let y = 200;;

let case_pacman = ref 0




let dessine_pac(x,y,c) = begin
    let oeil(x,y) = 
        set_color black;
        fill_circle (int_of_float (x+. 5.0)) (int_of_float (y+.7.0)) 4 in
    let bouche(x,y) =
        set_color white;
        fill_arc (int_of_float x) (int_of_float y)  20  20 (-20) 10  in
    set_color c;
    fill_circle (int_of_float x) (int_of_float y) 20;
    oeil(x,y);
    bouche(x,y);
   end
  
let mouvement(x,y) = begin
    let key = read_key() in
    match key with
    | 'z' -> (x,  (y + 30))
    | 's' -> (x,(y - 30))
    | 'q' -> ((x - 30 ),y) 
    | 'd' -> ((x + 30 ),y )
    | _ -> invalid_arg"wola"
    

    end



let redessine _ = begin
clear_graph ();
let (x,y) = mouvement(x,y); in
let (x,y) = (float_of_int x, float_of_int y) in
dessine_pac(x,(y +. 30.0),yellow)
end 
let  x = ref 300.0
let  y = ref 300.0
let () = 

dessine_pac(!x,!y,yellow);
while true do 
    print_float !x ;
    print_float !y ;
    let key = read_key() in
    let check(x,y) =
       
        match key with
            | 'z' -> y := !y +. 30.0 
            | 's' -> y := !y -. 30.0 
            | 'q' ->  x := !x -. 30.0 
            | 'd' ->  x := !x +. 30.0 
            | _ -> invalid_arg"wola"
        in
    check(x,y);
    clear_graph ();
    dessine_pac(!x,!y,yellow);
    
done


