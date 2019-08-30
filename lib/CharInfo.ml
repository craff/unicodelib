include Types
include Blocks

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


type width_context = EastAsian | Other

let width ?(context=Other) c =
  let i = Uchar.to_int c in
  try
    let inf = get_char_descr c in
    let gc = inf.general_category in
    if i = 0 then 0
    else if gc = Cc then -1
    else if gc = Me || gc = Mn then 0
    else if i = 0x007D (* SOFT HYPHEN *) then 1
    else if i = 0x200B (* ZERO WIDTH SPACE *) || gc = Cf then 0
    else if i >= 0x1160 && i <= 0x11FF (*Hangul Jamo medial vowels and final consonants *)
    then 0
    else if inf.east_asian_width = FullWidth || inf.east_asian_width = Wide
            || (context = EastAsian && inf.east_asian_width = Ambiguous) then 2
    else 1
  with Not_found -> 1

type previous_chars =
  EvenRegionalIndicator | ExtPictExtendStar | NoPrevious

(* previous must tel if the is an even number of RI before c1, if
   c1 is a RI. It should tell if there is a pattern
   ExtPict Extend* before c1 if c1 is ZWJ *)
let break_between previous bp1 bp2 =
  match (bp1,bp2) with
  | (CR, LF)                 -> false (* rule 3.0 *)
  | ((Control | CR | LF), _) -> true  (* rule 4.0 *)
  | (_, (Control | CR | LF)) -> true  (* rule 5.0 *)
  | (L, (L | V | LV | LVT))  -> false (* rule 6.0 *)
  | ((LV | V), (V | T))      -> false (* rule 7.0 *)
  | ((LVT | T), T)           -> false (* rule 8.0 *)
  | (_, (Extend | ZWJ))      -> false (* rule 9.0 *)
  | (_, SpacingMark)         -> false (* rule 9.1 *)
  | (Prepend, _)             -> false (* rule 9.2 *)
  | (ZWJ, ExtPict) when previous = ExtPictExtendStar
                             -> false (* rule 11.0 *)
  | (RegionalIndicator, RegionalIndicator) when previous = EvenRegionalIndicator
                             -> false (* rule 12.0 and 13.0, assuming *)

  | _                        -> true  (* rule 999.0 *)
