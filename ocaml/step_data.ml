#use "pcoord.ml";;
#use "pqueue.ml";;

(* The data that will be passed around from one A* step to another *)
type step_data = { loc : pcoord;
                   goal : pcoord;
				   closed : int array array;
				   queue : pqueue;
				   cost_arr : int array array;
				   heuristic_arr : int array array;
				   total_arr : int array array;
				   breadcrumb_arr : pcoord array array };;
