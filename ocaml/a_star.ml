(* Obviously, something has to invoke a_star *)

let rows = 15;;

let cols = 15;;

let max_iters = rows * cols / 2;;


(* def manhattanDistance(start: Coordinate,  end: Coordinate) : Int *)
let manhattan_distance start_loc end_loc =
    abs (start_loc.x - end_loc.x) + abs (start_loc.y - end_loc.y);;

	
(* def generateNewStepData(stepData: StepData, loc: Coordinate) : StepData *)
let generate_new_step_data stepdata location = 
	{ loc = location; goal = stepdata.goal; closed = stepdata.closed; queue = stepdata.queue;
	  cost_arr = stepdata.cost_arr; heuristic_arr = stepdata.heuristic_arr;
	  total_arr = stepdata.total_arr; breadcrumb_arr = stepdata.breadcrumb_arr };;


(* def retracePath(breadcrumbs: Array[Array[Coordinate]], current: Coordinate) : List[Coordinate] *)
let rec retrace_path breadcrumbs current_loc = 
	let next_loc = breadcrumbs.(current_loc.y).(current_loc.x) in
    if next_loc.x > -1 && next_loc.y > -1 then (retrace_path breadcrumbs next_loc) @ [current_loc]
    else [current_loc];;


let eat_breadcrumbs breadcrumbs goal =
	match retrace_path breadcrumbs breadcrumbs.(goal.y).(goal.x) with
	| h1::h2::tail -> Path.find_direction h1 h2
	| _ -> `Invalid;;

	  
(* def getFreshLoc(queue: PriorityQueue[PriorityCoordinate], closedSet: Array[Array[Boolean]]): PriorityCoordinate *)
let rec get_fresh_loc queue closed_set =
    match PQ.dequeue queue with
    | Some(maybe_loc) ->
        if closed_set.(maybe_loc.y).(maybe_loc.x) then (
            if not(PQ.is_empty queue) then get_fresh_loc queue closed_set
		    else {y = -1; x = -1; priority = -1}
        )
        else maybe_loc
    | None -> {y = -1; x = -1; priority = -1};;

	
(* def a_star_step(stepData: StepData) : StepData *)
let a_star_step stepdata =

    let loc = stepdata.loc in
    let goal = stepdata.goal in
    let closed_set = stepdata.closed in
    let queue = stepdata.queue in
    let cost_arr = stepdata.cost_arr in
    let heuristic_arr = stepdata.heuristic_arr in
    let total_arr = stepdata.total_arr in
    let breadcrumb_arr = stepdata.breadcrumb_arr in

    (Path.find_neighbors_of loc).iter (fun n ->

        let neighbor = Path.find_coords loc n in
        let x = neighbor.x in
        let y = neighbor.y in

        if not(closed_set.(y).(x)) then (

            let new_cost = cost_arr.(loc.y).(loc.x) + 1 in
            let contains_neighbor = PQ.mem queue neighbor in

            if not(contains_neighbor) || new_cost < cost_arr.(y).(x) then (
                cost_arr.(y).(x) <- new_cost;
                heuristic_arr.(y).(x) <- manhattan_distance neighbor goal;
                total_arr.(y).(x) <- cost_arr.(y).(x) + heuristic_arr.(y).(x);
                breadcrumb_arr.(y).(x) <- {y = loc.y; x = loc.x; priority = -1}
            );

            if not(contains_neighbor) then PQ.enqueue queue
                {y = neighbor.y; x = neighbor.x; priority = total_arr.(y).(x)}

        )

    );

    stepdata;;


(* StepData -> Int -> StepData *)
let rec a_star_iterate stepdata iters =
    if not(PQ.is_empty stepdata.queue) && iters < max_iters then (
        let loc = get_fresh_loc stepdata.queue stepdata.closed in
        if loc.y == stepdata.goal.y && loc.x == stepdata.goal.x then
            generate_new_step_data stepdata loc (* Exit point *)
        else (
            stepdata.closed.(loc.y).(loc.x) <- true;
            a_star_iterate (a_star_step (generate_new_step_data stepdata loc)) (iters + 1)
        )
	)
    else generate_new_step_data stepdata {y = -1; x = -1; priority = -1};; (* Exit point *)


let priority_comparator first second =
    match first with
    | None -> false
    | Some(f) -> match second with
                 | None -> false
                 | Some(s) -> f.priority < s.priority;;


let find_dir_with_a_star start goalie =
    let closed_set = Array.make_matrix cols rows false in
    let my_queue = PQ.make priority_comparator in
    let cost_vals = Array.make_matrix cols rows (-1) in
    let heuristic_vals = Array.make_matrix cols rows (-1) in
    let total_vals = Array.make_matrix cols rows (-1) in
    let breadcrumb_vals = Array.make_matrix cols rows {y = -1; x = -1; priority = -1} in

    cost_vals.(start.y).(start.x) <- 0;
    heuristic_vals.(start.y).(start.x) <- manhattan_distance start goalie;
    total_vals.(start.y).(start.x) <- cost_vals.(start.y).(start.x) + heuristic_vals.(start.y).(start.x);
    PQ.enqueue my_queue {y = start.y; x = start.x; priority = total_vals.(start.y).(start.x)};

    let stepdata = a_star_iterate
        { loc = start; goal = goalie; closed = closed_set; queue = my_queue;
          cost_arr = cost_vals; heuristic_arr = heuristic_vals;
          total_arr = total_vals; breadcrumb_arr = breadcrumb_vals} 0 in

    if stepdata.loc.x > -1 && stepdata.loc.y > -1 then
        eat_breadcrumbs stepdata.breadcrumb_arr stepdata.goal
    else
        `Invalid;;

