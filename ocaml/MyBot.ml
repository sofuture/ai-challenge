open Ants;;

(* 
             '-=.
           ,-"=. \
                \ \
             _,-=\/=._        _.-,_
            /         \      /=-._ "-.
           |   /~\   /~\    /     `-._\
           |   \o/   \o/   / K I L L  /        
            \_    ~~~ /    |  A L L  |
              `~,._,-'    /  A N T S /
                 | |      =-._      /
             _,-=/ \=-._     /|`-._/
           //           \\   )\
          /|             |)_.'/
         //|  M Y B O T  |\_."   _.-\
        (|  \           /    _.`=    \
        ||   ":_    _.;"_.-;"   _.-=.:
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

type food_distance = {
    distance: float;
    food: (int * int);
};;

type ant_food_distance = {
    ant: ant;
    food_distance: food_distance;
};;

type order = {
    subj: ant;
    dir: dir;
};;

(* helpers for initializing types *)

let dummy_food_dist = { distance = 1000.0; food = (0,0) };;

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

(* find how far all known food is from given ant *)
let food_distances state ant =
    let food_dist p1 = { distance = state#distance ant#loc p1; food = p1 } in
    List.map food_dist state#get_food;;

(* find food closest to given ant *)
let find_best_food_for_ant state ant =
    let food = food_distances state ant in
    let best_food = List.fold_left min_fd dummy_food_dist food in
    best_food;;

(* find which ant is closest to any food *)
let find_ant_closest_to_food state =
    let rec inner ants acc =
        match ants with
        | [] -> acc
        | h :: t -> 
            let best_food = find_best_food_for_ant state h in
            if best_food.distance < acc.food_distance.distance then 
                inner t {ant = h; food_distance = best_food}
            else 
                inner t acc in
    let best = inner state#my_ants {ant = new ant 0 0 0; food_distance =
        dummy_food_dist} in
    ddebug (Printf.sprintf "found the best food which is %f" best.food_distance.distance);;

(* --------- *)
(* ant logic *)
(* --------- *)

let step_ant state ant =
    let bf = find_best_food_for_ant state ant in
    let ((dd1, dd2), _) = state#distance_and_direction ant#loc bf.food in
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
        if List.mem coord acc then
            submit_orders state t acc
        else (
            if order.dir <> `Stop then
                state#issue_order (order.subj#loc, order.dir);
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
        submit_orders state orders [];
        state#finish_turn ()
    );;

loop mybot_engine;;
