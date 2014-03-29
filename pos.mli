type t

val create : int -> int -> t

val x : t -> int

val y : t -> int

val to_string : t -> string

val to_tuple : t -> int * int
