open UTFTypes

let unicodelib_db =
  try Sys.getenv "UNICODELIB_DB"
  with Not_found -> UnicodeConfig.unicodelib_db

let prefix_tree : prefix_tree Lazy.t =
  lazy (
      let m = PermanentMap.open_map unicodelib_db in
      let d = PermanentMap.get m (-1) in
      PermanentMap.close_map m; d)


let get_char_descr_from_file : Uchar.t -> char_description = fun u ->
  let m = PermanentMap.open_map unicodelib_db in
  let d = PermanentMap.get m (Uchar.to_int u) in
  PermanentMap.close_map m; d

let cache : (Uchar.t, char_description) Hashtbl.t = Hashtbl.create 2048
let get_char_descr : Uchar.t -> char_description = fun u ->
  try
    Hashtbl.find cache u
  with Not_found ->
    begin
      let d = get_char_descr_from_file u in
      Hashtbl.add cache u d; d
    end


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
