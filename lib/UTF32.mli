open CharInfo

val encode : Uchar.t -> string
val decode : string -> int -> (Uchar.t * int)
val validate : string -> bool
val fold : ('a -> Uchar.t -> 'a) -> 'a -> string -> 'a
val nth_index : string -> int -> int
val nth : string -> int -> (Uchar.t * int)
val next : string -> int -> int
val prev : string -> int -> int
val trim : string -> string
val init : int -> (int -> Uchar.t) -> string
val empty_string : string
val of_list : Uchar.t list -> string
val to_list : string -> Uchar.t list

module Buffer : sig
  include module type of Buffer

  val add_char : t -> Uchar.t -> unit

  val init : int -> (int -> Uchar.t) -> t
end

val nfc : string -> string
val nfd : string -> string
val nfkc : string -> string
val nfkd : string -> string
val custom_nfc : (decomposition_tag -> bool) -> string -> string
val custom_nfd : (decomposition_tag -> bool) -> string -> string
