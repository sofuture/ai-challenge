open Ants;;

(* Since using the proper passable function would be incorrect for the 
starter bot, this not_water function will be used instead. Note the use 
of the get_tile function. *)

type food_distance = {
    distance: float;
    food: (int * int);
};;

type ant_food_distance = {
    ant: ant;
    food_distance: food_distance;
};;

let dummy_food_dist = { distance = 1000.0; food = (0,0) };;

let not_water state loc =
   not ((state#get_tile loc) = `Water);;

let rec try_steps state ant dirs =
    match dirs with 
    | [] -> ()
    | `Stop :: tail ->
        try_steps state ant tail
    | d :: tail ->
        if not_water state (state#step_dir ant#loc d) then
            (* if we eat the food we want to remove it from our vision here *)
            state#issue_order (ant#loc, d)
        else try_steps state ant tail;;

let rec print_food flist =
    match flist with
    | [] -> ()
    | (dist, (row, col)) :: tail ->
            ddebug (Printf.sprintf "dist:%f (r:%d,c:%d)\n" dist row col);;

let food_distances state ant =
    let food_dist p1 = { distance = state#distance ant#loc p1; food = p1 } in
    List.map food_dist state#get_food;;

let find_best_food_for_ant state ant =
    let food = food_distances state ant in
    (* get the minimum distance food for this ant *)
    let min curr macc =
        if curr.distance < macc.distance then
            curr
        else 
            macc in
    let best_food = List.fold_left min dummy_food_dist food in
    best_food;;

let step_ant state ant =
    let bf = find_best_food_for_ant state ant in
    let ((dd1, dd2), _) = state#distance_and_direction ant#loc bf.food in
    try_steps state ant [dd1; dd2; `N; `E; `S; `W];;

let rec step_ants state my_l =
    match my_l with
    | [] -> ()
    | head :: tail ->
        step_ant state head;
        step_ants state tail;;

let find_ant_closest_to_food state =
    (* loop through ants *)
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


let mybot_engine state =
    if state#turn = 0 then state#finish_turn ()
    else
        state#update_vision;
        step_ants state state#my_ants;
        state#finish_turn ();;

loop mybot_engine;;
