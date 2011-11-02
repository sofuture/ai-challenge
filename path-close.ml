
open Hashtbl;;

open Printf;;

type ctype = [`Goal | `Origin | `Empty | `Blocked | `Seen];;
type direction = [`N | `S | `E | `W];;
type cell = ctype * int;;

let mwidth = 15;;
let mheight = 15;;

let map = 
    [[0;0;0;0;0;0;0;0;0;0;0;0;0;0;0];
    [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0];
    [0;0;1;0;0;0;0;0;0;0;0;0;0;0;0];
    [0;0;1;1;0;0;0;0;0;0;0;0;0;0;0];
    [0;0;1;1;1;1;0;0;0;0;0;0;0;0;0];
    [0;0;0;0;0;1;1;0;0;0;0;0;0;0;0];
    [0;0;0;0;0;0;1;0;0;0;0;0;0;0;0];
    [0;0;0;0;0;0;1;0;0;0;0;0;0;0;0];
    [0;0;0;0;0;0;1;0;0;0;0;0;9;0;0];
    [0;0;0;0;0;0;1;0;0;0;0;1;1;1;1];
    [0;0;0;0;0;0;1;0;0;0;0;0;0;0;0];
    [0;0;0;0;0;0;1;1;1;1;1;1;0;0;0];
    [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0];
    [0;0;8;0;0;0;0;0;0;0;0;0;0;0;0];
    [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0]];;

let navigate (orr, orc) dir =
    match dir with
    | `N -> (orr-1, orc)
    | `S -> (orr+1, orc)
    | `E -> (orr, orc+1)
    | `W -> (orr, orc-1)

let rec nav_dirs origin dirs =
    match dirs with
    | [] -> origin
    | h::t ->
        nav_dirs (navigate origin h) t;;

let cell_of_int ival = 
    if ival = 1 then `Blocked
    else if ival = 7 then `Seen
    else if ival = 8 then `Origin
    else if ival = 9 then `Goal
    else `Empty;;

let sym_of_cell cval =
    if cval = `Blocked then "*"
    else if cval = `Seen then "s"
    else if cval = `Origin then "O"
    else if cval = `Goal then "G"
    else " ";;

let rec print_point_list plist acc = 
    match plist with
    | [] -> Printf.printf "%s\n" acc
    | (hr,hc) :: t ->
        print_point_list t ( (Printf.sprintf "(%d,%d) " hr hc) ^ acc);;

let cells_from (r,c) =
    let pot = [ (r-1, c); (r, c-1); (r, c+1); (r+1, c); ] in
    let valid (fr,fc) = 
        (fr >= 0) && (fr < mheight) && (fc >= 0) && (fc < mwidth) in
    List.filter valid pot;;

let new_cells_from mdat loc known =
    let temp = cells_from loc in
    let valid el = 
        let (t,_) = Hashtbl.find mdat el in
        not(List.mem el known) && t <> `Blocked in
    List.filter valid temp;;

let print_map mdat = 
    for i = 0 to mheight - 1 do
        for j = 0 to mwidth - 1 do
            let (c,_) = Hashtbl.find mdat (i,j) in
            Printf.printf "%s" (sym_of_cell c)
        done;
        Printf.printf "\n"
    done;;

let populate_data () = 
    let origin = Hashtbl.create 1 in
    let goal = Hashtbl.create 1 in
    let dat = Hashtbl.create (mwidth * mheight) in
    let ar = map in
    for i = 0 to (List.length ar) - 1 do
        let row = List.nth ar i in
        for j = 0 to (List.length row) - 1 do
            let v = List.nth row j in
            let ctype = cell_of_int v in
            if ctype = `Origin then Hashtbl.add origin `Origin (i,j);
            if ctype = `Goal then Hashtbl.add goal `Goal (i,j);
            Hashtbl.add dat (i,j) (cell_of_int v, 0);
        done;
    done;
    (origin, goal, dat);;

let abs v = 
    if v < 0 then -v
    else v;;

let cost (orr,orc) (nr,nc) = 
    abs (orr - nr) + abs (orc - nc);;

let is_solution point goal =
    0 = cost point goal;;

let rec explore mdat frontier goal explored =
    match frontier with
    | [] -> false
    | h::t ->
        if is_solution h goal then (
            List.iter (fun l ->
                Hashtbl.replace mdat l (`Seen,0)) explored;
                print_map mdat;
            true
        ) else (
            if List.mem h explored then
                explore mdat t goal explored
            else (
                Printf.printf "yay\n";
                print_point_list [h] "";
                let nf = t @ (new_cells_from mdat h explored) in
                let sort p1 p2 = compare (cost p1 goal) (cost p2 goal) in 
                let sorted = List.sort sort nf in 
                explore mdat sorted goal (h::explored)
            )
        );;

let _ = 
    let (origin, goal, mdat) = populate_data () in
    let (orr,orc) = Hashtbl.find origin `Origin in
    let (gr,gc) = Hashtbl.find goal `Goal in
    Printf.printf "Origin at (%d,%d)\n" orr orc;
    Printf.printf "Goal at (%d,%d)\n" gr gc;
    if explore mdat [(orr,orc)] (gr,gc) [] then
        Printf.printf "WE CAN GET THERE!\n"
    else 
        Printf.printf "WE CANT GET THERE LOL!\n";;

