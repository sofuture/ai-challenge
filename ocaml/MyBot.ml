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

open Ants;;
open Queue;;

(* --------------- *)
(* strategy params *)
(* --------------- *)

(* hill guarding behavior *)

let minimum_ants_alive_per_hill_for_guarding = 5;;

let guards_per_hill = 3;;

(* -------------- *)
(* explicit types *)
(* -------------- *)

(* because i don't know how to use this from an external file
 * or more specifically, it doesnt work as expected *)

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

let find_n_closest_ants state ants loc n =
    let dsort ant ant2= 
        let ant_distance = state#distance2 loc ant#loc in
        let ant2_distance = state#distance2 loc ant2#loc in
        compare ant_distance ant2_distance in
    let sorted = List.sort dsort ants in
    let rec get_n inp acc =
        if List.length acc = n then 
            (acc, inp)
        else 
            match inp with
            | [] -> (acc, inp)
            | h::t -> get_n t (h::acc) in
    get_n sorted [];;

let string_of_role role = 
    match role with 
    | `Freelancer -> "`Freelancer"
    | `Guard -> "`Guard"
    | `Explorer -> "`Explorer"
    | `Warrior -> "`Warrior"
    | `Dead -> "`Dead";;
 
let rec print_ant_list ants =
    match ants with
    | [] -> ()
    | h::t ->
        let r, c = h#loc in
        let role = string_of_role h#role in
        ddebug (Printf.sprintf "(%d,%d) - %s\n" r c role);
        print_ant_list t;;

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

let step_ant_goal state ant goal =
    let ((dd1, dd2), _) = state#distance_and_direction ant#loc goal in
    let sh_dir = shuffle [dd1; dd2] in
    let sh_rem = shuffle [`N; `E; `S; `W] in
    let dirs = sh_dir @ sh_rem in
    try_steps state ant dirs;;

let step_ant state ant =
    let bf = find_best_move_for_ant state ant in
    step_ant_goal state ant bf.loc;;

(* ----------- *)
(* group logic *)
(* ----------- *)

(* get orders for all ants *)
let rec step_free_ants state my_l acc =
    match my_l with
    | [] -> acc
    | head :: tail ->
        let order = step_ant state head in
        step_free_ants state tail (order :: acc);;

(* get orders for guarding ants *)
let rec step_guard_ants state my_l acc =
    match my_l with
    | [] -> acc
    | (hill,ants) :: tail ->
        let rec single_guard acc ants =
            match ants with 
            | [] -> acc
            | h::t ->
                let order = step_ant_goal state h hill in
                single_guard (order :: acc) t in
        step_guard_ants state tail (single_guard [] ants)@acc;;

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

let should_we_guard state =
    let ant_count = List.length state#my_ants in
    let hill_count = List.length state#my_hills in
    ant_count >= (hill_count * minimum_ants_alive_per_hill_for_guarding);;

let give_roles state ants =
    if should_we_guard state then
        ([], ants)
    else
        let hills = state#my_hills in
        let fg (guards, left) (hill,_) =
            let guards_new, left_new = find_n_closest_ants state left hill guards_per_hill in
            ((hill, guards_new) :: guards, left_new) in    
        List.fold_left fg ([], ants) hills;;

(* ----------- *)
(* goooooooo!! *)
(* ----------- *)

let mybot_engine state =
    state#update_vision;
    state#reset_occupied;
    print_ant_list state#my_ants;
    if state#turn = 0 then (
        Random.self_init ();
        state#finish_turn ()
    ) else (
        ddebug (Printf.sprintf "\nabout to issue orders\n===================\n");
        let (guards, free) = give_roles state state#my_ants in
        let _ = step_free_ants state free [] in
        let _ = step_guard_ants state guards [] in
        ddebug (Printf.sprintf "time remaining: %f\n" state#time_remaining);
        state#finish_turn ()
    );;

loop mybot_engine;;
