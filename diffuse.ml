type map = float array array;;

let mheight = 50;;
let mwidth = 50;;

let p m =
    for i = 0 to (Array.length m) - 1 do
        for j = 0 to (Array.length m.(i)) - 1 do
            Printf.printf "%3d " (int_of_float m.(i).(j))
        done;
        Printf.printf "\n"
    done;;

let cells_from (r,c) =
    let pot = [ (r-1, c); (r, c-1); (r, c+1); (r+1, c); ] in
    let valid (fr,fc) = (fr >= 0) && (fr < mheight) && (fc >= 0) && (fc < mwidth) in
    List.filter valid pot;;

let diffusion_value mdat r c =
    let t = mdat.(r).(c) in
    let others = cells_from (r,c) in
    let sum_others acc (tr,tc) = acc +. mdat.(tr).(tc) in

    t +. (0.35 *. List.fold_left sum_others 0.0 others);;

let new_cells_from mdat loc explored =
    let valid el =
        let r, c = el in
        if Hashtbl.mem explored el then false
        else true in
    List.filter valid (cells_from loc);;

let rec diffuse mdat frontier explored =
    match frontier with
    | [] -> mdat
    | h :: t ->
        let r, c = h in
        if Hashtbl.mem explored h then
            diffuse mdat t explored
        else (
            Printf.printf "%d,%d\n" r c;
            Hashtbl.add explored h true;
            let next = new_cells_from mdat h explored in
            mdat.(r).(c) <- diffusion_value mdat r c;
            diffuse mdat (t@next) explored
        );;

let m = Array.make_matrix 50 50 0.0 in
let poi = [(10,20); (33,40)] in
m.(10).(20) <- 500.0;
m.(33).(40) <- 400.0;
List.fold_left (fun acc x -> diffuse acc [x] (Hashtbl.create 20)) m poi;
p m;;


