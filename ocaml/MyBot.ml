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
    | Some of 'a;;

type mtile = (int * int);;

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

let no_dudes state loc =
    let r, c = loc in
    let occupied = state#is_occupied loc in
    if occupied then
        ddebug (Printf.sprintf "checking %d,%d is occupied \n" r c)
    else
        ddebug (Printf.sprintf "checking %d,%d is not occupied \n" r c);
    not occupied;;

let valid_move state loc =
    no_dudes state loc && not_water state loc;;

(* try to move in the following dirs if possible (in order) 
 * return the move that works *)
let rec try_steps state ant dirs =
    match dirs with 
    | [] -> {subj = ant; dir = `Stop}
    | `Stop :: tail -> try_steps state ant tail
    | d :: tail ->
        let new_loc = state#step_dir ant#loc d in
        if valid_move state new_loc then
        (
            let orr, oc = ant#loc in
            let nr, nc = new_loc in
            ddebug (Printf.sprintf "leaving %d %d\n" orr oc);
            ddebug (Printf.sprintf "going to %d %d\n" nr nc);
            state#remove_occupied ant#loc;
            state#add_occupied new_loc;
            let order = {subj = ant; dir = d} in
            state#issue_order (order.subj#loc, order.dir);
            order
        ) else
            try_steps state ant tail;;

let get_tile_list state = 
    let for_rows acc row = acc @ Array.to_list row in
    Array.fold_left for_rows [] state#get_map;;

let get_type_tiles state ttype =
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
let thing_distances state ant ttypes =
    let rec inner types acc =
        match types with
        | [] -> acc
        | h :: t ->
            let tdist c = 
                match c with 
                | ((row:int), (col:int)) ->
                    ddebug (Printf.sprintf "thing at (%d, %d)\n" row col);
                    { 
                        ant = Some ant;
                        distance = state#distance ant#loc (row, col);
                        thing = Some h;
                        loc = row, col;
                    } in
            let dt = List.map tdist (get_type_tiles state h) in
            inner t (dt @ acc) in
    inner ttypes [];;
            
(* find how far all known food is from given ant *)
let food_distances state ant = thing_distances state ant [`Food];;

(* find food closest to given ant *)
let find_best_move_for_ant state ant =
    List.fold_left min_fd dummy_survey (thing_distances state ant [`Food; `Hill]);;

(* --------- *)
(* ant logic *)
(* --------- *)

let step_ant state ant =
    let bf = find_best_move_for_ant state ant in
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
        state#issue_order (order.subj#loc, order.dir);
        submit_orders state t acc;;
        
(* ----------- *)
(* goooooooo!! *)
(* ----------- *)

let mybot_engine state =
    if state#turn = 0 then (
        Random.self_init ();
        state#update_vision;
        state#reset_occupied;
        state#finish_turn ()
    ) else (
        state#update_vision;
        state#reset_occupied;
        ddebug (Printf.sprintf "\nabout to issue orders\n===================\n");
        let _ = step_ants state state#my_ants [] in
        ddebug (Printf.sprintf "time remaining: %f\n" state#time_remaining);
        state#finish_turn ()
    );;

loop mybot_engine;;
