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

let minimum_ants_alive_per_hill_for_guarding = 15;;

let guards_per_hill = 5;;

(* general tuning *)

let ignore_distance = 10.;;

let goal_close_enough = 5.;;

(* -------------- *)
(* explicit types *)
(* -------------- *)

type order = ant * dir;;

type hill_guards = location * ant list;;

type survey = ant option * float * tile option * location option;;

(* helpers for initializing types *)

let proto_survey = (None, 1000., None, (0,0));;

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

let min_distance curr acc =
    let (_, cd, _, _) = curr in
    let (_, ad, _, _) = acc in
    if cd < ad then curr
    else acc;;

(* game specific utils *)

let not_water state loc =
    not ((state#get_tile loc) = `Water);;

let no_dudes state loc =
    not (state#is_occupied loc);;

let valid_move state loc =
    no_dudes state loc && not_water state loc;;

(* try to move in the following dirs if possible (in order) 
 * return the move that works *)
let rec try_steps state ant dirs =
    match dirs with 
    | [] -> (ant, `Stop)
    | `Stop :: tail -> try_steps state ant tail
    | dir :: tail ->
        let new_loc = state#step_dir ant.loc dir in
        if valid_move state new_loc then
        (
            state#move_ant ant.loc dir new_loc;
            (ant, dir)
        ) else
            try_steps state ant tail;;

let get_tile_list state = 
    let for_rows acc row = acc @ Array.to_list row in
    Array.fold_left for_rows [] state#get_map;;

let get_type_tiles state ttype =
    let loc_of_loc_extra (l,o) = l in
    match ttype with 
    | `Goal -> []
    | `Water -> []
    | `Land -> []
    | `Food -> 
            (* (int * int) list *)
            state#get_food
    | `Ant -> 
            (* ((int * int) * int) list *)
            List.map loc_of_loc_extra state#enemy_ants
    | `Dead -> []
    | `Hill -> 
            (* ((int * int) * int) *)
            List.map loc_of_loc_extra state#enemy_hills
    | `Unseen -> [];;

let find_n_closest_ants state ants loc n =
    let dsort ant ant2 = 
        let ant_distance = state#distance2 loc ant.loc in
        let ant2_distance = state#distance2 loc ant2.loc in
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
        let r, c = h.loc in
        let role = string_of_role h.role in
        let s = match h.goal with
        | None -> Printf.sprintf "(%d,%d) - %s\n" r c role
        | Some (gr,gc) -> Printf.sprintf "(%d,%d) (goal:%d,%d)- %s\n" r c gr gc role in
        ddebug s;
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
                    let loc = (row, col) in 
                    (Some ant, state#distance ant.loc loc, Some h, loc) in
            let dt = List.map tdist (get_type_tiles state h) in
            inner t (dt @ acc) in
    inner ttypes [];;
            
(* find how far all known food is from given ant *)
let food_distances state ant = thing_distances state ant [`Food];;

(* find desired thing closest to given ant *)
let find_best_move_for_ant state ant =
    let distances = thing_distances state ant [`Food; `Hill] in
    let bt = List.fold_left min_distance proto_survey distances in
    let (bt_ant, bt_dist, bt_tile, bt_loc) = bt in    
    if bt_dist < ignore_distance then
        bt
    else 
        let g = match ant.goal with
        | None -> (0,0)
        | Some c -> 
            if (state#distance ant.loc c) < goal_close_enough then
                (state#new_goal_for ant)
            else
                c in
        (Some ant, 0.0, Some `Goal, g);;

(* --------- *)
(* ant logic *)
(* --------- *)

let step_ant_goal state ant goal =
    let ((dd1, dd2), _) = state#distance_and_direction ant.loc goal in
    let sh_dir = shuffle [dd1; dd2] in
    let sh_rem = shuffle [`N; `E; `S; `W] in
    let dirs = sh_dir @ sh_rem in
    try_steps state ant dirs;;

let step_ant state ant =
    let bf = find_best_move_for_ant state ant in
    let (_, _, _, loc) = bf in
    let r, c = loc in
    ddebug (Printf.sprintf "%d,%d\n" r c);
    step_ant_goal state ant loc;;

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

let should_we_guard state =
    let ant_count = List.length state#my_ants in
    let hill_count = List.length state#my_hills in
    ant_count > (hill_count * minimum_ants_alive_per_hill_for_guarding);;

let give_roles state ants =
    if not (should_we_guard state) then ([], ants)
    else 
        let hills = state#my_hills in
        let fg (guards, left) (hill,_) =
            let guards_new, left_new = find_n_closest_ants state left hill guards_per_hill in
            ((hill, guards_new) :: guards, left_new) in 
        List.fold_left fg ([], ants) hills;;

let print_ants ants = 
    let pant a =
        let r, c = a.loc in
        ddebug (Printf.sprintf "ant at %d %d\n" r c) in
    List.iter pant ants;;

(* ------------- *)
(* map diffusion *)
(* ------------- *)

let goal_type_tiles state gtype =
    match gtype with
    | `Food -> get_type_tiles state `Food
    | `Hills -> get_type_tiles state `Hill
    | _ -> [];;

let rec set_goals_for state types =
    match types with
    | [] -> ()
    | h::t ->
        let mint = Int32.to_int Int32.max_int in
        match h with
        | `Explore ->
            let m = state#get_map in
            for i = 1 to (Array.length m - 1) do
                for j = 1 to (Array.length m.(i) - 1) do
                    let t = m.(i).(j) in
                    let floc = (i, j) in
                    if not_water state (i, j) then (
                        let seen = t.seen in
                        let v = float_of_int (mint - ((200 - seen) * 300)) in
                        state#add_goal `Explore floc v
                    ) else (
                        let v = -1.0 in
                        state#add_goal `Explore floc v
                    )
                done;
            done;
            set_goals_for state t
        | _ ->
            let ts = goal_type_tiles state h in
            let add p = state#add_goal h p (float_of_int mint); () in
            List.iter add ts;
            set_goals_for state t;;

let set_goals state = 
    set_goals_for state [`Food;`Hills;`Explore];;

(* ----------- *)
(* goooooooo!! *)
(* ----------- *)

let mybot_engine state =
    state#update_vision;
    print_ant_list state#my_ants;
    if state#turn = 0 then (
        Random.self_init ();
        state#finish_turn ()
    ) else (
        set_goals state;
        state#diffuse;
        ddebug (Printf.sprintf "\nabout to issue orders\n===================\n");
        print_ants state#my_ants;
        let (guards, free) = give_roles state state#my_ants in
        let _ = step_free_ants state free [] in
        let _ = step_guard_ants state guards [] in
        ddebug (Printf.sprintf "time remaining: %f\n" state#time_remaining);
        state#finish_turn ()
    );;

loop mybot_engine;;
