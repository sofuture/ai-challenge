(*
 * Obviously, something has to invoke a_star
 *
 * Graph is tentatively needed 
 ** Will likely need Graph.isPassable(), .step(), .neighborsOf(), .findCoords(), .findDirection()
 *** For Terrain, look to ants.ml -> tile
 *** All references to Direction should go to ants.ml -> dir (and should avoid 'Stop)
 * 
 *)
   
(**FIX**)
val Rows = 15
val Cols = 15
val Max_Iters = Rows * Cols / 3

(* def a_star(start: Coordinate, goal: Coordinate) *)
let find_dir_with_a_star start goalie =

    closed_set = Array.make_matrix Cols Rows false;
    queue = Base.PriorityQueue.make (fun (_, _, p1) (_, _, p2) -> p1 < p2);

    cost_vals = Array.make_matrix Cols Rows -1;
    heuristic_vals = Array.make_matrix Cols Rows -1;
    total_vals = = Array.make_matrix Cols Rows -1;
    breadcrumb_vals = = Array.make_matrix Cols Rows {y = -1; x = -1; priority = -1};

	cost_vals.(start.y).(start.x) = 0;
	heuristic_vals.(start.y).(start.x) = manhattan_distance start goalie;
	total_vals.(start.y).(start.x) = cost_vals.(start.y).(start.x) + heuristic_vals.(start.y).(start.x);

    queue.add {y = start.y; x = start.x; priority = total_vals.(start.y).(start.x)};

    stepdata = a_star_iterate {loc = start; goal = goalie; closed = closed_set; pqueue = queue;
	                           cost_arr = cost_vals; heuristic_arr = heuristic_vals; total_arr = total_vals; breadcrumb_arr = breadcrumb_vals} 0;
	
    if ((stepdata.loc.x > -1) && (stepdata.loc.y > -1)) then eat_breadcrumbs stepdata.breadcrumb_arr stepdata.goal
	else ();;

}


(* def a_star_iterate(stepData: StepData, iters: Int) : StepData *)
let rec a_star_iterate stepdata iters =
    if (not(stepdata.queue.is_empty) && (iters < Max_Iters)) then (
	
		loc = get_fresh_loc stepdata.queue stepdata.closed;

        if ((loc.y == stepdata.goal.y) && (loc.x == stepdata.goal.x)) then generate_new_step_data stepData loc; (* Exit point *) 

        stepdata.closed.(loc.y).(loc.x) = true;
		a_star_iterate (a_star_step (generate_new_step_data stepdata loc)) (iters + 1);
	
	);
	else generate_new_step_data stepdata {y = -1; x = -1; priority = -1};; (* Exit point *)

	
(* def generateNewStepData(stepData: StepData, loc: Coordinate) : StepData *)
let generate_new_step_data stepdata location = 
	{ loc = location; goal = stepdata.goal; closed = stepdata.closed; pqueue = stepdata.pqueue;
	  cost_arr = stepdata.cost_arr; heuristic_arr = stepdata.heuristic_arr;
	  total_arr = stepdata.total_arr; breadcrumb_arr = stepdata.breadcrumb_arr };;

	  
(* def getFreshLoc(queue: PriorityQueue[PriorityCoordinate], closedSet: Array[Array[Boolean]]): PriorityCoordinate *)
let get_fresh_loc queue closed_set
	maybe_loc = queue.first;
	while closed_set.(maybe_loc.y).(maybe_loc.x) do
		if (not(queue.is_empty)) then maybe_loc = queue.first;
		else maybe_loc = ();
	done;
	maybe_loc;;

	
(* def a_star_step(stepData: StepData) : StepData *)
let a_star_step stepdata =

    loc = stepdata.loc;
    goal = stepdata.goal;
    closed_set = stepdata.closed;
    queue = stepdata.pqueue;
    cost_arr = stepdata.cost_arr;
    heuristic_arr = stepdata.heuristic_arr;
    total_arr = stepdata.total_arr;
    breadcrumb_arr = stepdata.breadcrumb_arr;

    (**FIX**) (graph.neighborsOf loc).iter (fun n ->

        neighbor = (**FIX**) Graph.findCoords loc n;
        x = neighbor.x;
        y = neighbor.y;

        if (not(closed_set.(y).(x))) then (

            new_cost = cost_arr.(loc.y).(loc.x) + 1;
            contains_neighbor = queue.mem neighbor;

            if (not(contains_neighbor) || (new_cost < cost_arr.(y).(x))) then (
                cost_arr(x)(y) = new_cost;
                heuristic_arr(x)(y) = manhattan_distance neighbor goal;
                total_arr(x)(y) = cost_arr.(y).(x) + heuristic_arr.(y).(x);
                breadcrumb_arr(x)(y) = {y = loc.y; x = loc.x; -1};
            );

            if (not(contains_neighbor)) then queue.add {y = neighbor.y; x = neighbor.x; total_arr.(y).(x)};

        );

    );

    stepData;;


(* def manhattanDistance(start: Coordinate,  end: Coordinate) : Int *)
let manhattan_distance start_loc end_loc =
    abs (start_loc.x - end_loc.x) + abs (start_loc.y - end_loc.y);;

	
(* def eatBreadcrumbs(breadcrumbs: Array[Array[Coordinate]], goal: Coordinate, graph: Graph) *)
let eat_breadcrumbs breadcrumbs goal = 
	match retrace_path breadcrumbs breadcrumbs.(goal.y).(goal.x) with
	| h1::h2::tail -> (**FIX**)Graph.find_direction(h1, h2)
	| _ -> ();;

	
(* def retracePath(breadcrumbs: Array[Array[Coordinate]], current: Coordinate) : List[Coordinate] *)
let retrace_path breadcrumbs current_loc = 
	next_loc = breadcrumbs.(current_loc.y)(current_loc.x);
    if ((next_loc.x > -1) && (next_loc.y > -1)) then (retrace_path breadcrumbs next_loc)::[current_loc]
    else [current_loc];;