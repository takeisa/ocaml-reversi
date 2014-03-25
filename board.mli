type t

val create : unit -> t

val put : t -> Pos.t -> Cell.t -> t

val get : t -> Pos.t -> Cell.t

val init : unit -> t

val width : int

val height : int

val can_put : t -> Pos.t -> Disk.t -> bool

val put_and_reverse : t -> Pos.t -> Disk.t -> t

val pos_list : t -> Disk.t -> Pos.t list

val count_disk : t -> Disk.t -> int
