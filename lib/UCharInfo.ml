open UTFTypes

let get_char_descr : Uchar.t -> char_description = fun u ->
  Hashtbl.find UnicodeData.data u

let general_category : Uchar.t -> general_category = fun u ->
  let d = get_char_descr u in
  d.general_category

let unicode_name : Uchar.t -> string = fun u ->
  let d = get_char_descr u in
  String.concat " " d.name

let to_lower : Uchar.t -> Uchar.t option = fun u ->
  let d = get_char_descr u in
  d.lowercase

let to_upper : Uchar.t -> Uchar.t option = fun u ->
  let d = get_char_descr u in
  d.uppercase

(*
 * Tells whether the given unicode character corresponds to a spacing
 * character, including regular space, tabulation, newline, ...
 *)
let is_space : Uchar.t -> bool = fun c ->
  let c = Uchar.to_int c in
  (0x0009 <= c && c <= 0x000d) || c = 0x0020 || c = 0x00a0
  || c = 0x1680 || c = 0x180e || (0x2000 <= c && c <= 0x200a)
  || c = 0x202f || c = 0x205f || c = 0x3000
