open Hashtbl;;
open Printf;;

type cell = [`Goal | `Origin | `Empty | `Blocked];;

let mwidth = 15;;
let mheight = 15;;

let map = 
    [[0;0;0;0;0;0;0;0;0;0;0;0;0;0;0];
    [0;0;0;0;0;0;0;0;0;0;0;0;0;9;0];
    [0;0;1;0;0;0;0;0;0;0;0;0;0;0;0];
    [0;0;1;1;0;0;0;0;0;0;0;0;0;0;0];
    [0;0;1;1;1;1;0;0;0;0;0;0;0;0;0];
    [0;0;0;0;0;1;1;0;0;0;0;0;0;0;0];
    [0;0;0;0;0;0;1;0;0;0;0;0;0;0;0];
    [0;0;0;0;0;0;1;0;0;0;0;0;0;0;0];
    [0;0;0;0;0;0;1;0;0;0;0;0;0;0;0];
    [0;0;0;0;0;0;1;0;0;0;0;1;1;1;1];
    [0;0;0;0;0;0;1;0;0;0;0;1;0;0;0];
    [0;0;0;0;0;0;1;0;1;1;1;1;0;0;0];
    [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0];
    [0;0;8;0;0;0;0;0;0;0;0;0;0;0;0];
    [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0]];;

let cell_of_int ival = 
    if ival = 1 then `Blocked
    else if ival = 8 then `Origin
    else if ival = 9 then `Goal
    else `Empty;;

let sym_of_cell cval =
    if cval = `Blocked then "*"
    else if cval = `Origin then "O"
    else if cval = `Goal then "G"
    else " ";;

let cells_from (r,c) =
    let pot = 
        [ (r-1, c-1); (r-1, c); (r-1, c+1);
          (r,   c-1);           (r,   c+1);
          (r+1, c-1); (r+1, c); (r+1, c+1) ] in

    let valid (fr,fc) = 
        (fr >= 0) && (fr < mheight) && (fc >= 0) && (fc < mwidth) in
    List.filter valid pot;;

let print_map mdat = 
    for i = 0 to mheight - 1 do
        for j = 0 to mwidth - 1 do
            Printf.printf "%s" (sym_of_cell (Hashtbl.find mdat (i,j)))
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
            Hashtbl.add dat (i,j) (cell_of_int v);
        done;
    done;
    (origin, goal, dat);;

let (origin, goal, mdat) = populate_data () in
let (orr,orc) = Hashtbl.find origin `Origin in
let (gr,gc) = Hashtbl.find goal `Goal in
Printf.printf "Origin at (%d,%d)\n" orr orc;
Printf.printf "Goal at (%d,%d)\n" gr gc;
print_map mdat
