val out_chan : out_channel
val get_time : unit -> float
val ddebug : string -> unit
type 'a option = None | Some of 'a
type 'a cache = Invalid | Valid of 'a
type role = [ `Dead | `Explorer | `Freelancer | `Guard | `Warrior ]
type dir = [ `E | `N | `S | `Stop | `W ]
type tile =
    [ `Ant | `Dead | `Food | `Goal | `Hill | `Land | `Unseen | `Water ]
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
}
type mapb = { content : int; seen : int; row : int; col : int; }
type location = int * int
type loc_extra = location * int
type ant = {
  role : role;
  loc : location;
  owner : int;
  goal : location option;
  lseen : int;
}
type order = location * dir
type goal_type = [ `EnemyAnts | `Explore | `Food | `Hills ]
type goal_map = goal_type * location list * float array array
type tgame_state = {
  setup : game_setup;
  turn : int;
  go_time : float;
  tmap : mapb array array;
  my_ants : (location, ant) Hashtbl.t;
  food : (location, mapb) Hashtbl.t;
  my_hills : (location, loc_extra) Hashtbl.t;
  enemy_hills : (location, loc_extra) Hashtbl.t;
  enemy_ants : loc_extra list;
  cache_my_ants : ant list cache;
  cache_food : location list cache;
  cache_my_hills : loc_extra list cache;
  cache_enemy_hills : loc_extra list cache;
  goal_maps : (goal_type, goal_map) Hashtbl.t;
  cells_from : (location, location list) Hashtbl.t;
}
val proto_tile : mapb
val tile_of_int :
  int -> [> `Ant | `Dead | `Food | `Goal | `Hill | `Land | `Unseen | `Water ]
val string_of_dir : [< `E | `N | `S | `Stop | `W ] -> string
val int_of_tile :
  [< `Ant | `Dead | `Food | `Goal | `Hill | `Land | `Unseen | `Water ] -> int
val random_location : tgame_state -> int * int
val visible : tgame_state -> int * int -> bool
val ht_to_key_list : 'a -> 'b -> 'a list -> 'a list
val ht_to_val_list : 'a -> 'b -> 'b list -> 'b list
val tile_of : int -> int -> int -> int -> mapb
val set_turn : tgame_state -> int -> tgame_state
val set_loadtime : tgame_state -> int -> tgame_state
val set_turntime : tgame_state -> int -> tgame_state
val set_rows : tgame_state -> int -> tgame_state
val set_cols : tgame_state -> int -> tgame_state
val set_turns : tgame_state -> int -> tgame_state
val set_viewradius2 : tgame_state -> int -> tgame_state
val set_attackradius2 : tgame_state -> int -> tgame_state
val set_spawnradius2 : tgame_state -> int -> tgame_state
val set_player_seed : tgame_state -> int -> tgame_state
val uncomment : string -> string
val sscanf_cps :
  ('a, Scanf.Scanning.scanbuf, 'b, 'c -> 'd, 'a -> 'e, 'e) format6 ->
  'c -> (string -> 'd) -> string -> 'd
val add_food : tgame_state -> int -> int -> tgame_state
val remove_food : tgame_state -> int -> int -> tgame_state
val add_water : tgame_state -> int -> int -> tgame_state
val clear_tile : mapb -> int -> int -> mapb
val clear_gstate : tgame_state -> tgame_state
val add_hill : tgame_state -> int -> int -> int -> tgame_state
val add_ant : tgame_state -> int -> int -> int -> tgame_state
val new_goal_for : tgame_state -> ant -> int * int
val is_occupied_location : tgame_state -> location -> bool
val initialize_map : tgame_state -> tgame_state
val add_line : tgame_state -> string -> tgame_state
val update : tgame_state -> string list -> tgame_state
val read_lines : unit -> string list option
val read : tgame_state -> tgame_state option
val issue_order : (int * int) * [< `E | `N | `S | `Stop | `W ] -> unit
val move_ant :
  tgame_state ->
  location -> [< `E | `N | `S | `Stop | `W ] -> location -> unit
val finish_turn : unit -> unit
val step_unbound : [< `E | `N | `S | `Stop | `W ] -> int * int -> int * int
val wrap0 : int -> int -> int
val wrap_bound : int * int -> int * int -> int * int
val step_dir :
  [< `E | `N | `S | `Stop | `W ] -> int * int -> int * int -> int * int
val get_tile :
  mapb array array ->
  int * int ->
  [> `Ant | `Dead | `Food | `Goal | `Hill | `Land | `Unseen | `Water ]
val shorter_dist : int -> int -> int -> bool * int
val stepdistance_ndirection :
  int * int ->
  int * int ->
  int * int -> ([> `N | `S | `Stop ] * [> `E | `Stop | `W ]) * (int * int)
val direction : int * int -> int * int -> int * int -> dir * dir
val fsquare_int : int -> float
val distance2 : int * int -> int * int -> int * int -> int
val distance : int * int -> int * int -> int * int -> float
val distance_and_direction :
  int * int ->
  int * int ->
  int * int -> ([> `N | `S | `Stop ] * [> `E | `Stop | `W ]) * float
val mark_seen : int -> int * int -> mapb array array -> unit
val paint_fov : ant -> tgame_state -> unit
val update_vision : ant list -> tgame_state -> unit
val passable : tgame_state -> int * int -> bool
val centre : tgame_state -> int * int
val time_remaining : tgame_state -> float
val remove_dead_ants : tgame_state -> unit
val add_goal : tgame_state -> goal_type -> location -> float -> unit
val cells_from : tgame_state -> int * int -> int * int -> location list
val diffusion_value :
  tgame_state -> float array array -> int * int -> int * int -> float
val new_cells_from :
  tgame_state ->
  'a -> int * int -> (location, 'b) Hashtbl.t -> int * int -> location list
val diffuse :
  tgame_state ->
  float array array ->
  location list -> (location, bool) Hashtbl.t -> float array array
val print_diffuse_map : float array array -> unit
class swrap :
  tgame_state ->
  object
    val mutable state : tgame_state
    method add_goal : goal_type -> location -> float -> unit
    method bounds : int * int
    method centre : int * int
    method clear_goals : unit
    method diffuse : unit
    method direction : int * int -> int * int -> dir * dir
    method distance : int * int -> int * int -> float
    method distance2 : int * int -> int * int -> int
    method distance_and_direction :
      int * int -> int * int -> (dir * dir) * float
    method enemy_ants : loc_extra list
    method enemy_hills : loc_extra list
    method finish_turn : unit -> unit
    method get_food : location list
    method get_map : mapb array array
    method get_player_seed : int
    method get_state : tgame_state
    method get_tile : int * int -> tile
    method goal_maps : (goal_type, goal_map) Hashtbl.t
    method invalidate_caches : unit
    method is_occupied : location -> bool
    method issue_order : order -> unit
    method move_ant : location -> dir -> location -> unit
    method my_ants : ant list
    method my_hills : loc_extra list
    method new_goal_for : ant -> int * int
    method passable : int * int -> bool
    method set_state : tgame_state -> unit
    method step_dir : int * int -> dir -> int * int
    method time_remaining : float
    method turn : int
    method update_vision : unit
    method visible : int * int -> bool
  end
val loop : (swrap -> 'a) -> unit
