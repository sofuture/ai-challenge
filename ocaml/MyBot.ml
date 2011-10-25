open Ants;;

(* 
             '-=.          K I L L
           ,-"=. \          A L L
                \ \        A N T S
             _,-=\/=._        .
            /         \      /\
           |   /~\   /~\     /\ 
           |   \o/   \o/     /\        
            \_    ~~~ /      ||
              `~,._,-'       / \
                 | |        =-.=
             _,-=/ \=-._     /|
           //           \\   )\
          /|             |)_.'/
         //|  M Y B O T  |\_."   _.-\
        (|  \           /    _.`=    \
        ||   ":_    _.;"_.-""   _.-=.:
     _-."/    / `-."\_."        =-_.;\
    `-_./   /             _.-=.    / \\
           |              =-_.;\ ."   \\
           \                   \\/     \\
           /\_                .'\\      \\
          //  `=_         _.-"   \\      \\
         //      `~-.=`"`'       ||      ||
         ||    _.-_/|            ||      |\_.-_
     _.-_/|   /_.-._/            |\_.-_  \_.-._\
    /_.-._/                      \_.-._\
    
*)


(* -------------- *)
(* explicit types *)
(* -------------- *)

type 'a option =
    | None
    | Some of 'a

type mtile = (int * int) 

type survey = {
    ant: ant option;
    distance: float;
    thing: tile option;
    loc: int * int;
};;

type order = {
    subj: ant;
    dir: dir;
};;

(* helpers for initializing types *)

let dummy_survey = {
    ant = None;
    distance = 1000.0;
    thing = None;
    loc = 0, 0;
};;

(* ------------- *)
(* general utils *)
(* ------------- *)

let shuffle l =
    let ar = Array.of_list l in
    for n = Array.length ar - 1 downto 1 do
        let k = Random.int (n+1) in
        let temp = ar.(k) in 
            ar.(k) <- ar.(n);
            ar.(n) <- temp
    done;
    Array.to_list ar;;

let min_fd curr acc =
    if curr.distance < acc.distance then
        curr
    else
        acc;;

(* game specific utils *)

let not_water state loc =
    not ((state#get_tile loc) = `Water);;

let valid_move state loc =
    not_water state loc;;

(* try to move in the following dirs if possible (in order) 
 * return the move that works *)
let rec try_steps state ant dirs =
    match dirs with 
    | [] -> {subj = ant; dir = `Stop}
    | `Stop :: tail -> try_steps state ant tail
    | d :: tail ->
        if valid_move state (state#step_dir ant#loc d) then
            {subj = ant; dir = d}
        else 
            try_steps state ant tail;;

let get_tile_list state = 
    let for_rows acc row = acc @ Array.to_list row in
    Array.fold_left for_rows [] state#get_map;;

let get_tile_list_of_type state ttype =
    (*
     * too inefficient to use
     *
    let mf cell = 
        if tile_of_int cell.content = ttype then true
        else false in
    List.filter mf (get_tile_list state);;
    *)
    match ttype with 
    | `Water -> []
    | `Land -> []
    | `Food -> 
            (* (int * int) list *)
            state#get_food
    | `Ant -> 
            (* Ants.ant list -> (int * int) list*)
            List.map (fun x -> x#loc) state#enemy_ants
    | `Dead -> []
    | `Hill -> 
            (* ((int * int) * int) *)
            List.map (fun ((r,c),o) -> (r,c)) state#enemy_hills
    | `Unseen -> [];;


(* find how far all known <thing> is from given ant *)
let thing_distances state ant ttype =
    let tdist c = 
        match c with 
        | ((row:int), (col:int)) ->
            ddebug (Printf.sprintf "thing at (%d, %d)\n" row col);
            { 
                ant = Some ant;
                distance = state#distance ant#loc (row, col);
                thing = Some ttype;
                loc = row, col;
            } in
    List.map tdist (get_tile_list_of_type state ttype);;

(* find how far all known food is from given ant *)
let food_distances state ant = thing_distances state ant `Food;;

(* find food closest to given ant *)
let find_best_food_for_ant state ant =
    let food = food_distances state ant in
    List.fold_left min_fd dummy_survey food;;

(* --------- *)
(* ant logic *)
(* --------- *)

let step_ant state ant =
    let bf = find_best_food_for_ant state ant in
    let ((dd1, dd2), _) = state#distance_and_direction ant#loc bf.loc in
    let sh_dir = shuffle [dd1; dd2] in
    let sh_rem = shuffle [`N; `E; `S; `W] in
    let dirs = sh_dir @ sh_rem in
    try_steps state ant dirs;;

(* ----------- *)
(* group logic *)
(* ----------- *)

(* get orders for all ants *)
let rec step_ants state my_l acc =
    match my_l with
    | [] -> acc
    | head :: tail ->
        let order = step_ant state head in
        step_ants state tail (order :: acc);;

let rec submit_orders state orders acc =
    match orders with
    | [] -> ()
    | order :: t ->
        let coord = state#step_dir order.subj#loc order.dir in
        let atr, atc = order.subj#loc in
        let r, c = coord in
        ddebug (Printf.sprintf "at (%d,%d) want to move to (%d, %d)\n" atr atc r c);
        if List.mem coord acc then (
            ddebug (Printf.sprintf "at (%d,%d) cant move to (%d, %d)\n" atr atc r c);
            submit_orders state t (order.subj#loc :: acc)
        )
        else (
            ddebug (Printf.sprintf "at (%d,%d) okay to move to (%d, %d)\n" atr atc r c);
            if order.dir <> `Stop then (
                state#issue_order (order.subj#loc, order.dir);
                (* if we just moved, remove our current location from 
                 * forbidden spots *)
                let less_at = List.filter (fun (rx,ry) -> rx <> atr && rx <> atc) acc in
                submit_orders state t (coord :: less_at)
            ) else 
                submit_orders state t (coord :: acc)
        );;

(* ----------- *)
(* goooooooo!! *)
(* ----------- *)

let mybot_engine state =
    if state#turn = 0 then state#finish_turn ()
    else (
        state#update_vision;
        let orders = step_ants state state#my_ants [] in
        ddebug (Printf.sprintf "\nabout to issue orders\n===================\n");
        let curr = List.map (fun x -> x.subj#loc) orders in
        submit_orders state orders curr;
        state#finish_turn ()
    );;

loop mybot_engine;;
