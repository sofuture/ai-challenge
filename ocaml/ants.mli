val out_chan : out_channel
val get_time : unit -> float
val ddebug : string -> unit
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
type role = [ `Dead | `Explorer | `Freelancer | `Guard | `Warrior ]
class ant :
  row:int ->
  col:int ->
  owner:int ->
  role:role ->
  object
    method col : int
    method loc : int * int
    method owner : int
    method role : role
    method row : int
    method to_string : string
  end
type tgame_state = {
  setup : game_setup;
  turn : int;
  my_ants : ant list;
  enemy_ants : ant list;
  my_hills : ((int * int) * int) list;
  enemy_hills : ((int * int) * int) list;
  dead_ants : ant list;
  food : (int * int) list;
  tmap : mapb array array;
  go_time : float;
  now_occupied : (int * int, int) Hashtbl.t;
  mmap : (int * int, mapb) Hashtbl.t;
}
type dir = [ `E | `N | `S | `Stop | `W ]
type tile = [ `Ant | `Dead | `Food | `Hill | `Land | `Unseen | `Water ]
type order = (int * int) * dir
val proto_tile : mapb
val tile_of_int :
  int -> [> `Ant | `Dead | `Food | `Hill | `Land | `Unseen | `Water ]
val string_of_dir : [< `E | `N | `S | `Stop | `W ] -> string
val int_of_tile :
  [< `Ant | `Dead | `Food | `Hill | `Land | `Unseen | `Water ] -> int
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
val reset_occupied : tgame_state -> unit
val remove_occupied_location : tgame_state -> int * int -> unit
val add_occupied_location : tgame_state -> int * int -> unit
val is_occupied_location : tgame_state -> int * int -> bool
val add_dead_ant : tgame_state -> int -> int -> int -> tgame_state
val initialize_map : tgame_state -> tgame_state
val add_line : tgame_state -> string -> tgame_state
val update : tgame_state -> string list -> tgame_state
val read_lines : unit -> string list option
val read : tgame_state -> tgame_state option
val issue_order : (int * int) * [< `E | `N | `S | `Stop | `W ] -> unit
val finish_turn : unit -> unit
val step_unbound : [< `E | `N | `S | `Stop | `W ] -> int * int -> int * int
val wrap0 : int -> int -> int
val wrap_bound : int * int -> int * int -> int * int
val step_dir :
  [< `E | `N | `S | `Stop | `W ] -> int * int -> int * int -> int * int
val get_tile :
  mapb array array ->
  int * int -> [> `Ant | `Dead | `Food | `Hill | `Land | `Unseen | `Water ]
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
val paint_fov : < loc : int * int; .. > -> tgame_state -> unit
val update_vision : < loc : int * int; .. > list -> tgame_state -> unit
val visible : tgame_state -> int * int -> bool
val passable : tgame_state -> int * int -> bool
val centre : tgame_state -> int * int
val time_remaining : tgame_state -> float
class swrap :
  tgame_state ->
  object
    val mutable state : tgame_state
    method add_occupied : int * int -> unit
    method bounds : int * int
    method centre : int * int
    method direction : int * int -> int * int -> dir * dir
    method distance : int * int -> int * int -> float
    method distance2 : int * int -> int * int -> int
    method distance_and_direction :
      int * int -> int * int -> (dir * dir) * float
    method enemy_ants : ant list
    method enemy_hills : ((int * int) * int) list
    method finish_turn : unit -> unit
    method get_food : (int * int) list
    method get_map : mapb array array
    method get_player_seed : int
    method get_state : tgame_state
    method get_tile : int * int -> tile
    method is_occupied : int * int -> bool
    method issue_order : order -> unit
    method my_ants : ant list
    method my_hills : ((int * int) * int) list
    method passable : int * int -> bool
    method remove_occupied : int * int -> unit
    method reset_occupied : unit
    method set_state : tgame_state -> unit
    method step_dir : int * int -> dir -> int * int
    method time_remaining : float
    method turn : int
    method update_vision : unit
    method visible : int * int -> bool
  end
val loop : (swrap -> 'a) -> unit
