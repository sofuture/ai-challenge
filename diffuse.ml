type map = float array array;;

let mheight = 20;;
let mwidth = 20;;

let p m =
    for i = 0 to (Array.length m) - 1 do
        for j = 0 to (Array.length m.(i)) - 1 do
            Printf.printf "%4f " m.(i).(j)
        done;
        Printf.printf "\n"
    done;;

let cells_from (r,c) =
    let pot = [ (r-1, c); (r, c-1); (r, c+1); (r+1, c); ] in
    let valid (fr,fc) = (fr >= 0) && (fr < mheight) && (fc >= 0) && (fc < mwidth) in
    List.filter valid pot;;

let diffusion_value mdat r c =
    let t = mdat.(r).(c) in
    if t > 0.0 then (
        let others = cells_from (r,c) in
        let sum_others acc (tr,tc) = acc +. mdat.(tr).(tc) in
        t +. (0.12 *. List.fold_left sum_others 0.0 others)
    ) else (
        0.0
    );;

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
            Hashtbl.add explored h true;
            let next = new_cells_from mdat h explored in
            mdat.(r).(c) <- diffusion_value mdat r c;
            diffuse mdat (t@next) explored
        );;

let m = Array.make_matrix mheight mwidth 1.0 in
for i = 0 to (mheight-4) do
    m.(i).(13) <- -1.;
done;
p m;
let poi = [(5,10); (7,15)] in
m.(5).(10) <- 999.0;
m.(7).(15) <- 999.0;
List.fold_left (fun acc x -> diffuse acc [x] (Hashtbl.create 20)) m poi;
List.fold_left (fun acc x -> diffuse acc [x] (Hashtbl.create 20)) m poi;
p m;;


