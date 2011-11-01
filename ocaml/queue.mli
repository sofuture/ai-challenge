type 'a queue = ('a list * 'a list) ref
module Queue :
  sig
    exception Empty
    val create : unit -> ('a list * 'b list) ref
    val add : ('a list * 'b) ref -> 'a -> unit
    val take : ('a list * 'a list) ref -> 'a
    val length : ('a list * 'b list) ref -> int
  end
