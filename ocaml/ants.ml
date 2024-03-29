(*
 * Started as the OCaml starter bot package
 *
 * Shit is about to get tore up
 *)

open Unix;;

open Hashtbl;;

(* ----------------------- *)
(* debugging utility stuff *)
(* ----------------------- *)

let out_chan = open_out "mybot_err.log";;

let get_time () = Unix.gettimeofday ();;

(* our local setup runs our bot with -lol to log *)
let ddebug s =
    if Array.length Sys.argv > 1 then (
        output_string out_chan s; 
        flush out_chan
    ) else ();;

(* --------------- *)
(* types and enums *)
(* --------------- *)

type 'a option =
    | None
    | Some of 'a;;

type 'a cache = 
    | Invalid
    | Valid of 'a;;

type role = [`Freelancer | `Guard | `Explorer | `Warrior | `Dead];;

type dir = [`N | `E | `S | `W | `Stop];;

type tile = [`Water | `Land | `Food | `Ant | `Dead | `Hill | `Unseen | `Goal];;

type game_setup = {
    loadtime : int;
    turntime : int;
    rows : int;
    cols : int;
    turns : int;
    viewradius2 : int;
    attackradius2 : int;
    spawnradius2 : int;
    player_seed : int;
};;

type mapb = {
    content : int;
    seen : int;
    row : int;
    col : int;
};;

type location = int * int;;

type loc_extra = location * int;;

type ant = { 
    role : role;
    loc : location;
    owner : int;
    goal : location option;
    lseen : int;
};;

type order = location * dir;;

type goal_type = [`Food | `Hills | `Explore | `EnemyAnts];;

type goal_map = goal_type * (location, bool) Hashtbl.t * float array array;;

type tgame_state = {

    (* general *)
    setup : game_setup;
    turn : int;
    go_time: float;

    (* map *)
    tmap: mapb array array; 

    (* things *)
    my_ants : (location, ant) Hashtbl.t;
    food : (location, mapb) Hashtbl.t;
    my_hills : (location, loc_extra) Hashtbl.t;
    enemy_hills : (location, loc_extra) Hashtbl.t;
    enemy_ants : loc_extra list;

    (* cached values *)
    cache_my_ants : ant list cache;
    cache_food : location list cache;
    cache_my_hills : loc_extra list cache;
    cache_enemy_hills : loc_extra list cache;

    goal_maps : (goal_type, goal_map) Hashtbl.t;

    cells_from : (location, location list) Hashtbl.t;
};;

let proto_tile = {
    content = 0;
    seen = 0;
    row = 0;
    col = 0;
};;

let tile_of_int c =
    if c = 0 then `Unseen
    else if c = 1 then `Land
    else if c = 2 then `Water
    else if c = 3 then `Food
    else if (c > 99) && (c < 200) then `Ant
    else if (c > 299) && (c < 400) then `Hill
    else if c = 1000 then `Goal
    else `Dead;;

let string_of_dir d = 
    match d with
    | `N -> "N" 
    | `E -> "E"
    | `S -> "S"
    | `W -> "W"
    | `Stop -> "Stop";;

let int_of_tile t =
    match t with
    | `Unseen -> 0
    | `Land -> 1
    | `Water -> 2
    | `Food -> 3
    | `Ant -> 199
    | `Dead -> 999
    | `Hill -> 399
    | `Goal -> 1000

let random_location state = 
    let r = Random.int (Array.length state.tmap) in
    let c = Random.int (Array.length state.tmap.(0)) in
    (r,c);;

(* Is tile at loc visible this turn? *)
let visible gstate (row, col) =
    gstate.tmap.(row).(col).seen = gstate.turn;;
        
let ht_to_key_list k v acc = k :: acc;;

let ht_to_val_list k v acc = v :: acc;;

let tile_of t seen row col =
    { content = t; seen = seen; row = row; col = col };;

let set_turn gstate v =
    { gstate with turn = v };;

let set_loadtime gstate v =
    { gstate with setup = {gstate.setup with loadtime = v} };;

let set_turntime gstate v = 
    { gstate with setup = {gstate.setup with turntime = v} };;

let set_rows gstate v = 
    { gstate with setup = {gstate.setup with rows = v} };;

let set_cols gstate v = 
    { gstate with setup = {gstate.setup with cols = v} };;

let set_turns gstate v = 
    { gstate with setup = {gstate.setup with turns = v} };;

let set_viewradius2 gstate v = 
    { gstate with setup = {gstate.setup with viewradius2 = v} };;

let set_attackradius2 gstate v = 
    { gstate with setup = {gstate.setup with attackradius2 = v} };;

let set_spawnradius2 gstate v = 
    { gstate with setup = {gstate.setup with spawnradius2 = v} };;

let set_player_seed gstate v = 
    { gstate with setup = {gstate.setup with player_seed = v} };;

let uncomment s =
    try 
        String.sub s 0 (String.index s '#')
    with Not_found -> s;;

let sscanf_cps fmt cont_ok cont_fail s =
    try 
        Scanf.sscanf s fmt cont_ok
    with _ -> 
        cont_fail s;;

let add_food gstate row col =
    let f = int_of_tile `Food in
    gstate.tmap.(row).(col) <- { gstate.tmap.(row).(col) with content = f};
    Hashtbl.add gstate.food (row, col) (tile_of f gstate.turn row col);
    gstate;;

let remove_food gstate row col =
    if gstate.tmap.(row).(col).content = (int_of_tile `Food) then
        gstate.tmap.(row).(col) <- { gstate.tmap.(row).(col) with 
            content = (int_of_tile `Land)};
    Hashtbl.remove gstate.food (row, col);
    gstate;;

let add_water gstate row col =
    gstate.tmap.(row).(col) <- {gstate.tmap.(row).(col) with content = (int_of_tile `Water)};
    gstate;;

let clear_tile t row col =
    match (tile_of_int t.content) with
    | `Water | `Unseen -> t
    | _ -> 
        {t with content = (int_of_tile `Land); row = row; col = col};;

let clear_gstate gs =
    if gs.turn < 1 then 
        gs 
    else (
        for count_row = 0 to (Array.length gs.tmap - 1) do
            let test_row = gs.tmap.(count_row) in
            for count_col = 0 to (Array.length test_row - 1) do
                if visible gs (count_row, count_col) then (
                    Hashtbl.remove gs.food (count_row, count_col);
                    Hashtbl.remove gs.my_hills (count_row, count_col);
                    Hashtbl.remove gs.enemy_hills (count_row, count_col);
                );
                test_row.(count_col) <- clear_tile test_row.(count_col) count_row count_col
            done
        done;
        {gs with enemy_ants = []});;

let add_hill gstate row col owner =
    try (
        gstate.tmap.(row).(col) <- { gstate.tmap.(row).(col) with content = (300 + owner) };
        match owner with
        | 0 ->
            Hashtbl.add gstate.my_hills (row, col) ((row, col), owner);
            gstate
        | n ->
            Hashtbl.add gstate.enemy_hills (row, col) ((row, col), owner);
            gstate
    ) with _ -> gstate;;

let add_ant gstate row col owner =
    try (
        let loc = (row, col) in
        gstate.tmap.(row).(col) <- { gstate.tmap.(row).(col) with content = (100 + owner) };
        let goal = random_location gstate in
        let new_ant = {
            lseen = gstate.turn;
            role = `Freelancer; 
            loc = loc; 
            owner = owner; 
            goal = Some goal } in
        match owner with
        | 0 ->
            if not (Hashtbl.mem gstate.my_ants loc) then
                Hashtbl.add gstate.my_ants loc new_ant
            else  (
                let old_ant = Hashtbl.find gstate.my_ants loc in
                Hashtbl.replace gstate.my_ants loc {old_ant with lseen = gstate.turn}
            );
            gstate
        | n ->
            {gstate with enemy_ants = ((row, col), owner) :: gstate.enemy_ants}
    ) with _ -> gstate;;

let new_goal_for gstate ant =
    let loc = ant.loc in
    let old_ant = Hashtbl.find gstate.my_ants loc in
    let new_goal = random_location gstate in
    Hashtbl.replace gstate.my_ants loc {old_ant with goal = Some new_goal};
    new_goal;;

let is_occupied_location gstate loc =
    try (
        Hashtbl.mem gstate.my_ants loc
    ) with _ -> false;;

let initialize_map gstate =
    let new_map = Array.make_matrix gstate.setup.rows gstate.setup.cols proto_tile in
    for count_row = 0 to (Array.length new_map - 1) do
        let test_row = new_map.(count_row) in
        for count_col = 0 to (Array.length test_row - 1) do
            test_row.(count_col) <- clear_tile test_row.(count_col) count_row count_col
        done
    done;
    {gstate with tmap = new_map};;

let add_line gstate line =
    sscanf_cps "%s %d %d %d"
        (fun ad row col owner ->
            match ad with
            | "a" -> add_ant gstate row col owner
(*            | "d" -> add_dead_ant gstate row col owner*)
            | "h" -> add_hill gstate row col owner
            | bd -> gstate)
        (sscanf_cps "%s %d %d"
            (fun fw row col ->
                match fw with
                | "f" -> add_food gstate row col
                | "w" -> add_water gstate row col
                | "r" -> remove_food gstate row col
                | _ -> gstate)
            (sscanf_cps "%s %d"
                (fun key v ->
                    match key with
                    | "turn" -> set_turn gstate v
                    | "loadtime" -> set_loadtime gstate v
                    | "turntime" -> set_turntime gstate v
                    | "rows" -> set_rows gstate v
                    | "cols" -> set_cols gstate v
                    | "turns" -> set_turns gstate v
                    | "viewradius2" -> set_viewradius2 gstate v
                    | "attackradius2" -> set_attackradius2 gstate v
                    | "spawnradius2" -> set_spawnradius2 gstate v
                    | "player_seed" -> set_player_seed gstate v
                    | _ -> gstate)
                (fun (line : string) ->
                  gstate)
            )
        )
        (uncomment line);;

let update gstate lines =
    let cgstate = 
        if gstate.turn = 0 then
            gstate
        else
            clear_gstate gstate in
    let ugstate = List.fold_left add_line cgstate lines in 
    if ugstate.turn = 0 then
        if ugstate.setup.rows < 0 || ugstate.setup.cols < 0 then
            (
            ddebug "\nBad setup info! Expect crashes!\n\n";
            ugstate
            )
        else 
            initialize_map ugstate
    else 
        ugstate;;

let read_lines () =
    let rec read_loop acc =
        let line = read_line () in
        if String.length line >= 2 && String.sub line 0 2 = "go" 
            || String.length line >= 3 && String.sub line 0 3 = "end"
            || String.length line >= 5 && String.sub line 0 5 = "ready" then
            (List.rev acc)
        else
            read_loop (line :: acc) in
    try 
        Some (read_loop []) 
    with End_of_file -> 
        None;;

let read gstate =
    let ll = read_lines () in
    let go_time = get_time () in
    match ll with
    | Some lines -> 
            Some {(update gstate lines) with go_time = go_time}
    | None -> None;;

let issue_order ((row, col), cdir) =
    let os = Printf.sprintf "o %d %d %s\n" row col (string_of_dir cdir) in
    Printf.printf "%s" os;;

let move_ant gstate f_loc dir t_loc =
    let a = Hashtbl.find gstate.my_ants f_loc in
    Hashtbl.remove gstate.my_ants f_loc;
    Hashtbl.add gstate.my_ants t_loc { a with loc = t_loc; };
    issue_order (f_loc, dir);;

(* Print go, newline, and flush buffer *)
let finish_turn () = 
    Printf.printf "go\n%!";;

let step_unbound d (row, col) =
    match d with
    | `N -> (row - 1), col
    | `S -> (row + 1), col
    | `W -> row, (col - 1)
    | `E -> row, (col + 1)
    | `Stop -> row, col;;

let rec wrap0 bound n =
    if bound < 0 then 
        (ddebug (Printf.sprintf "wrap0 below zero not allowed%!"); 0)
    else if n < 0 then 
        wrap0 bound (n + bound)
    else if n >= bound then 
        wrap0 bound (n - bound)
    else 
        n;;

let wrap_bound (rows, cols) (row, col) =
    wrap0 rows row, 
    wrap0 cols col;;

(* tell me my target co-ordinates when I step in a direction *)
let step_dir d bounds (row, col) =
    let new_loc = step_unbound d (row, col) in
    wrap_bound bounds new_loc;;

(* return the tile type at a location *)
let get_tile tmap (row, col) =
    try
        tile_of_int (tmap.(row).(col)).content
    with e -> 
        ddebug (Printf.sprintf "\nocaml Ants warning: exception getting tile %d, %d: %s\n" row col (Printexc.to_string e));
        `Unseen;;

(* shortest distance from point 1 to point 2: is it "inside", and how far? *)
let shorter_dist w p1 p2 =
    let d1 = abs (p2 - p1) in
    let d2 = w - d1 in
    (d1 < d2), (min d1 d2);;

(* see distance_and_direction below *)
let stepdistance_ndirection (rows, cols) (row1, col1) (row2, col2) =
    let row_si, row_dist = shorter_dist rows row1 row2 in
    let col_si, col_dist = shorter_dist cols col1 col2 in
    let row_dir =
        if row1 = row2 then `Stop
        else
            match row_si with 
            | true ->
                if row1 < row2 then `S
                else `N
            | false ->
                if row1 < row2 then `N
                else `S in
    let col_dir =
        if col1 = col2 then `Stop
        else
        match col_si with 
        | true ->
            if col1 < col2 then `E
            else `W
        | false ->
            if col1 < col2 then `W
        else `E in 
    (row_dir, col_dir), (row_dist, col_dist);;

(* returns d, has a type declaration for some reason *)
let direction bounds p1 p2 =
    let d, _ = stepdistance_ndirection bounds p1 p2 in
    (d: (dir * dir));;

let fsquare_int i =
    let f = float_of_int i in f *. f;;

(* distance squared *)
let distance2 (rows, cols) (src_row, src_col) (dst_row, dst_col) =
    let d1 = abs (src_row - dst_row) in
    let d2 = abs (src_col - dst_col) in
    let dr = min d1 (rows - d1) in
    let dc = min d2 (cols - d2) in
    (dr * dr) + (dc * dc);;

(* distance (not squared) *)
let distance b p1 p2 = 
    sqrt (float_of_int (distance2 b p1 p2));;

(* returns the distance and the two directions you might travel in to 
get from p1 to p2 ignoring water, with `Stop(s) for none *)
let distance_and_direction bounds p1 p2 =
    let d, (r, c) = stepdistance_ndirection bounds p1 p2 in
    d, (sqrt ((fsquare_int r) +. (fsquare_int c)));;

let mark_seen turn (pr, pc) tmap =
    tmap.(pr).(pc) <- { tmap.(pr).(pc) with seen = turn };;

(* Draw a filled vision circle around an ant *)
let paint_fov ant gstate =
    let c_row, c_col = ant.loc in
    let bounds = gstate.setup.rows, gstate.setup.cols in
    let r2 = gstate.setup.viewradius2 in
    let r = int_of_float (sqrt (float_of_int r2)) in
    let ul_row, ul_col = wrap_bound bounds (c_row - r, c_col - r) in
    for count_rows = 0 to (r * 2) do
        for count_cols = 0 to (r * 2) do
            let pr, pc = wrap_bound bounds (ul_row + count_rows, ul_col + count_cols) in
            if distance2 bounds (pr, pc) ant.loc <= r2 then
                mark_seen gstate.turn (pr, pc) gstate.tmap
        done
    done;;

(* Update vision status for all ants *)
let rec update_vision my_ants gstate =
    match my_ants with
    | [] -> ()
    | head :: tail ->
         paint_fov head gstate;
         update_vision tail gstate;;

(* How many milliseconds remain? *)
let time_remaining state = 
    let turn_time = 
        if state.turn = 0 then 
            (float_of_int state.setup.loadtime)
         else 
             (float_of_int state.setup.turntime) in
      1000. *. ((turn_time /. 1000.) -. ((get_time ()) -. state.go_time));;

let remove_dead_ants gstate =
    let turn = gstate.turn in
    let inner k v acc =
        if v.lseen = turn then acc
        else k::acc in
    let dead_keys = Hashtbl.fold inner gstate.my_ants [] in
    let rem k =
        Hashtbl.remove gstate.my_ants k in
    List.iter rem dead_keys

let rec add_goal gstate gtype location value =
    let r,c = location in
    if Hashtbl.mem gstate.goal_maps gtype then (
        let map = Hashtbl.find gstate.goal_maps gtype in
        let (_, locs, mat) = map in
        mat.(r).(c) <- value;
        Hashtbl.add locs location true;
        Hashtbl.replace gstate.goal_maps gtype (gtype, locs, mat)
    ) else (
        let locs = Hashtbl.create 20 in
        Hashtbl.add locs location true;
        let mat = Array.make_matrix gstate.setup.rows gstate.setup.cols 1.0 in
        Hashtbl.add gstate.goal_maps gtype (gtype, locs, mat);
        add_goal gstate gtype location value
    );;


let cells_from gstate (r,c) (mheight, mwidth) =
    if Hashtbl.mem gstate.cells_from (r,c) then
        Hashtbl.find gstate.cells_from (r,c)
    else (
        let pot = [ (r-1, c); (r, c-1); (r, c+1); (r+1, c); ] in
        let valid (fr,fc) = (fr >= 0) && (fr < mheight) && (fc >= 0) && (fc < mwidth) in
        let res = List.filter valid pot in
        Hashtbl.add gstate.cells_from (r,c) res;
        res
    );;

let diffusion_value gstate mdat (r,c) bounds =
    let t = mdat.(r).(c) in
    if t > 0.0 then (
        let others = cells_from gstate (r,c) bounds in
        let sum_others acc (tr,tc) = acc +. mdat.(tr).(tc) in
        t +. (0.12 *. List.fold_left sum_others 0.0 others)
    ) else (
        0.0
    );;

let new_cells_from gstate mdat loc frontier explored bounds =
    let valid el =
        if Hashtbl.mem explored el then false
        else true in
    List.filter valid (cells_from gstate loc bounds);;

let print_diffuse_map map =
    if true then ()
    else 
        for i = 0 to (Array.length map) - 1 do
            for j = 0 to (Array.length map.(i)) - 1 do
                ddebug (Printf.sprintf "%4f " map.(i).(j))
            done;
            ddebug "\n"
        done;;

let loop_diffuse gstate mdat n = 
    let bounds = (gstate.setup.rows, gstate.setup.cols) in
    for a = 0 to n do
        for i = 0 to (Array.length mdat - 1) do
            for j = 0 to (Array.length mdat.(i) - 1) do
                mdat.(i).(j) <- diffusion_value gstate mdat (i,j) bounds
            done;
        done;
    done;
    ();;

let rec diffuse gstate mdat frontier explored =
    loop_diffuse gstate mdat 100;;
(*
    else 
        match frontier with
        | [] -> mdat
        | h :: t ->
            if Hashtbl.mem explored h then
                diffuse gstate mdat t explored
            else (
                let r, c = h in
                let bounds = (gstate.setup.rows, gstate.setup.cols) in
                Hashtbl.add explored h true;
                let next = new_cells_from gstate mdat h frontier explored bounds in
                mdat.(r).(c) <- diffusion_value gstate mdat (r,c) bounds;
                diffuse gstate mdat (t@next) explored
            );;
*)

let diffuse_explore gstate mdat =
    let mint = Int32.to_int Int32.max_int in
    for i = 0 to (Array.length mdat - 1) do
        for j = 0 to (Array.length mdat.(i) - 1) do
            let seen = gstate.tmap.(i).(j).seen in
            let v = 
                if tile_of_int gstate.tmap.(i).(j).content = `Water then -1.0
                else float_of_int (mint - ((200 - seen) * 300)) in
            mdat.(i).(j) <- v
        done;
    done;
    loop_diffuse gstate mdat 10;
    ();;

class swrap state =
    object (self)
    val mutable state = state

        method bounds = state.setup.rows, state.setup.cols

        method issue_order (o:order) = issue_order o
        
        method finish_turn () = finish_turn ()
        
        method direction p1 p2 = ((direction self#bounds p1 p2): (dir * dir))
        
        method step_dir loc (d:dir) = step_dir d self#bounds loc
        
        method get_tile loc = ((get_tile state.tmap loc): tile)
        
        method distance2 p1 p2 = distance2 self#bounds p1 p2
        
        method distance p1 p2 = distance self#bounds p1 p2
        
        method distance_and_direction p1 p2 = 
            ((distance_and_direction self#bounds p1 p2): ((dir * dir) * float))
        
        method update_vision = 
            update_vision (Hashtbl.fold ht_to_val_list state.my_ants []) state
        
        method visible loc = visible state loc
        
        method time_remaining = time_remaining state
        
        method set_state s = state <- s
        
        method get_state = state
        
        method turn = state.turn
        
        method get_map = state.tmap
        
        method get_player_seed = state.setup.player_seed
        
        method is_occupied loc = is_occupied_location state loc
        
        method move_ant loc1 (d:dir) loc2 = move_ant state loc1 d loc2
        
        method enemy_ants = state.enemy_ants
        
        method new_goal_for ant = new_goal_for state ant

        method goal_maps = state.goal_maps

        method add_goal gtype location value = add_goal state gtype location value
        
        method diffuse = 
            let diffuse_one k (ttype, loc_list, map) =
                match ttype with 
                | `Explore ->
                    ddebug (Printf.sprintf "start diffuse explore %f\n" (time_remaining state));
                    diffuse_explore state map;
                    ddebug (Printf.sprintf "end diffuse explore %f\n" (time_remaining state))
                | _ -> (
                    ddebug (Printf.sprintf "start diffuse %f\n" (time_remaining state));
                    loop_diffuse state map 10;
 (*                   let locs = Hashtbl.fold ht_to_key_list loc_list [] in
                    let _res = diffuse state map locs loc_list in*)
                    ddebug (Printf.sprintf "end diffuse %f\n" (time_remaining state));
                    () 
                )in
            Hashtbl.iter diffuse_one state.goal_maps

        method my_ants = 
            match state.cache_my_ants with
            | Valid c -> c
            | Invalid ->
                let nc = Hashtbl.fold ht_to_val_list state.my_ants [] in
                state <- {state with cache_my_ants = Valid nc};
                nc

        method get_food =
            match state.cache_food with
            | Valid c -> c
            | Invalid ->
                let nc = Hashtbl.fold ht_to_key_list state.food [] in
                state <- {state with cache_food = Valid nc};
                nc

        method my_hills = 
            match state.cache_my_hills with
            | Valid c -> c
            | Invalid ->
                let nc = Hashtbl.fold ht_to_val_list state.my_hills [] in
                state <- {state with cache_my_hills = Valid nc};
                nc

        method enemy_hills =
            match state.cache_enemy_hills with
            | Valid c -> c
            | Invalid ->
                let nc = Hashtbl.fold ht_to_val_list state.enemy_hills [] in
                state <- {state with cache_enemy_hills = Valid nc};
                nc

        method invalidate_caches =
            remove_dead_ants state;
            state <- {state with
                cache_my_ants = Invalid;
                cache_food = Invalid;
                cache_my_hills = Invalid;
                cache_enemy_hills = Invalid;}

    end;;

(* Main game loop. Bots should define a main function taking a swrap for 
an argument (see above), and then call loop main_function. See how the 
starter bot in MyBot.ml does it if this doesn't make sense.
   This loop function will exit the program, killing your bot if an 
exception is raised. This is good for debugging, but should be changed 
before release. Calling the finish_turn function might be a good 
alternative. *)

let loop engine =

    let proto_setup = {
        loadtime = -1;
        turntime = -1;
        rows = -1;
        cols = -1;
        turns = -1;
        viewradius2 = -1;
        attackradius2 = -1;
        spawnradius2 = -1;
        player_seed = 932463947; } in

    let proto_gstate = {
        setup = proto_setup;
        turn = 0;
        tmap = Array.make_matrix 1 1 proto_tile; 
        go_time = 0.0;
        enemy_ants = [];
        my_ants = Hashtbl.create 20;
        food = Hashtbl.create 20;
        my_hills = Hashtbl.create 10;
        enemy_hills = Hashtbl.create 20;
        goal_maps = Hashtbl.create 3;
        cache_my_ants = Invalid;
        cache_food = Invalid;
        cache_my_hills = Invalid;
        cache_enemy_hills = Invalid;
        cells_from = Hashtbl.create 2000;
    } in

    for count_row = 0 to (Array.length proto_gstate.tmap - 1) do
        let test_row = proto_gstate.tmap.(count_row) in
        for count_col = 0 to (Array.length test_row - 1) do
            test_row.(count_col) <- clear_tile test_row.(count_col) count_row count_col
        done
    done;

    let wrap = new swrap proto_gstate in
    let rec take_turn i gstate = 
        match read gstate with 
        | Some state ->
            begin try (
                wrap#set_state state;
                wrap#invalidate_caches;
                engine wrap;
                flush Pervasives.stdout;
            ) with exc -> (
                ddebug (Printf.sprintf 
                "Exception in turn %d :\n" i);
                ddebug (Printexc.to_string exc);
                raise exc
            ) end;
            take_turn (i + 1) wrap#get_state
        | None -> () in 
    take_turn 0 proto_gstate;;
