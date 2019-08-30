(** {1 Module to manipulate UTF8 encoded string} *)

open CharInfo

(** [encode c]: git the string encoding [c] in UTF8 *)
val encode : Uchar.t -> string

(** [decode s offset] git the unicode  character at position [offset] in [s] and
    the offset of the next character in the string *)
val decode : string -> int -> (Uchar.t * int)

(** [validate s] returns true iff [s] is a valid UTF8 string *)
val validate : string -> bool

(** [fold  fn a  s] gives [f  ... (f  a c1) ...  cn] is [s]  is composed  of the
unicode characters [c1] ... [cn] *)
val fold : ('a -> Uchar.t -> 'a) -> 'a -> string -> 'a

(** [nth_index s n] gives the index of the [n]th uncode character in [s]. If [n]
is  negative,  count from  the  end  ([nth_index s  (-1)]  is  the last  unicode
character in [s] *)
val nth_index : string -> int -> int

(** [nth s n] gives the [n]th unicode character in [s] and its length in UTF8. *)
val nth : string -> int -> (Uchar.t * int)

(**  [nth  s offset]  Gives  the  offset of  the  next  unicode character  after
[offset] *)
val next : string -> int -> int

(** [prev  s offset] Gives  the offset of  the previous unicode  character after
[offset] *)
val prev : string -> int -> int

(** [trim s] removes all spaces at beginning and end of [s] *)
val trim : string -> string

(** [init  len fn]  creates a UTF8  encoded string of  length [len]  whose [n]th
character is [fn n] *)
val init : int -> (int -> Uchar.t) -> string

(** Conversion between UTF8 encoded string and list of unicode characters *)
val of_list : Uchar.t list -> string
val to_list : string -> Uchar.t list

(** Version  of Ocaml's [Buffer] module  with [add_char] and [init]  modified to
deal with UTF8 encode unicode charaters *)
module Buffer : sig
  include module type of Buffer

  val add_char : t -> Uchar.t -> unit

  val init : int -> (int -> Uchar.t) -> t
end

(** Standard normalization functions *)
val nfd : string -> string
val nfc : string -> string
val nfkd : string -> string
val nfkc : string -> string

(**  [custom_nfd  allowed  s]  will   decompose  all  unicode  characters  whose
decomposition tag is evalued to true by [allowed] *)
val custom_nfd : (decomposition_tag -> bool) -> string -> string

(**  [custom_nfc  allowed  s]  will   decompose  all  unicode  characters  whose
decomposition tag is evalued to true  by [allowed] and then recompose characters
according to the canonical equivalence.  [custom_nfc allowed s = nfc [custom_nfd
allowed s)].
 *)
val custom_nfc : (decomposition_tag -> bool) -> string -> string

(** [grapheme_break s n] returns true is position n is between two
    distinct grapheme *)
val grapheme_break : string -> int -> bool

(** [next_grapheme s n] returns the postion of the next grapheme.
    raise Not_found at the end of the string.  *)
val next_grapheme : string -> int -> int

(** [fold_graphement fn acc s] applies fn to acc and all the grapheme
    is s starting by the first one (like List.fold_left) *)
val fold_grapheme : (string -> 'a -> 'a) -> 'a -> string -> 'a
