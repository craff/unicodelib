open Unicodelib

let tbl1 = ref []
let tbl2 = ref []
let tbl3 = ref []
let outfile = Sys.argv.(1)

let _ =
  let open CharInfo in
  let c = ref Uchar.min in

  try while true do
    let w = width !c in
    if Uchar.to_int !c mod 100 = 0 then Printf.printf "\r%x/%x    %!"
      (Uchar.to_int !c) (Uchar.to_int Uchar.max);
    begin
      match !tbl1 with
      | ((x,_),w')::l when w = w' -> tbl1 := ((x,!c),w')::l
      | _                         -> tbl1 := ((!c,!c),w)::!tbl1
    end;
    let w = width ~context:EastAsian !c in
    begin
      match !tbl2 with
      | ((x,_),w')::l when w = w' -> tbl2 := ((x,!c),w')::l
      | _                         -> tbl2 := ((!c,!c),w)::!tbl2
    end;
    let gb = try (get_char_descr !c).grapheme_break
             with Not_found -> Other
    in
    begin
      match !tbl3 with
      | ((x,_),gb')::l when gb = gb' -> tbl3 := ((x,!c),gb')::l
      | _                            -> tbl3 := ((!c,!c),gb)::!tbl3
    end;
    c := Uchar.succ !c
      done
  with _ -> ()

let split l =
  let n = List.length l in
  let rec fn acc n l =
    if n = 0 then (List.rev acc, l) else
      match l with
      | [] -> assert false
      | x::l -> fn (x::acc) (n-1) l
  in
  fn [] ((n+1)/2) l

open Ast_helper
open Longident
open Parsetree

let loc = Location.none

let rec expr : ('a -> expression)
  -> ((Uchar.t * Uchar.t) * 'a) list -> expression = fun fn l -> match l with
    | [] -> assert false
    | [(_,w)] -> fn w
    | l ->
       let (l1,l2) = split l in
       let x = match l2 with
         | [] -> assert false
         | ((_,x),_)::_ -> x
       in
       [%expr if n <= [%e Exp.constant (Const.int (Uchar.to_int x))] then
             [%e expr fn l2] else [%e expr fn l1]]

let exp_from_int w = Exp.constant (Const.int w)
let exp_cname s = Exp.construct (Location.mknoloc (Lident (CharInfo.grapheme_break_to_string s))) None

let str_items = [%str
  type context = ASCII | UTF8 | CJK_UTF8
  let width ?(context=UTF8) c =
    let n = Uchar.to_int c in
    if context = CJK_UTF8 then
      [%e expr exp_from_int !tbl2]
    else
      [%e expr exp_from_int !tbl1]
  type grapheme_break_property =
    | Other
    | CR
    | LF
    | Prepend
    | Control
    | Extend
    | SpacingMark
    | L | V | T | LV | LVT
    | ZWJ
    | RegionalIndicator
    | ExtPict
  let gbp c =
    let n = Uchar.to_int c in
    [%e expr exp_cname !tbl3]

  type previous_chars =
    EvenRegionalIndicator | ExtPictExtendStar | NoPrevious

  (* [break_between previous c1 c2] returns true if and only if a grapheme break
   is between c1 and c2.
   previous must tel if there is an even number of RI before c1, if
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

  (*
   * Encode a unicode character into a UTF8 string.
   * Argument:
   *   i : the unicode character.
   * Returns a string of size between 1 and 4.
   * Raise invalid_arg if i is not in the U+0000..U+10FFFF range.
   *)
  let encode : Uchar.t -> string = fun i ->
    let i = Uchar.to_int i in
    if i < 0 then
      raise (invalid_arg "UF8.encode")
    else if i <= 0x7F then
      String.make 1 (char_of_int i)
    else if i <= 0x077F then
      let c0 = char_of_int (((i lsr 6) land 0b00011111) lor 0b11000000) in
      let c1 = char_of_int ((i         land 0b00111111) lor 0b10000000) in
      let s = Bytes.create 2 in
      Bytes.set s 0 c0; Bytes.set s 1 c1;
      Bytes.to_string s
    else if i <= 0xFFFF then
      let c0 = char_of_int (((i lsr 12) land 0b00001111) lor 0b11100000) in
      let c1 = char_of_int (((i lsr 6)  land 0b00111111) lor 0b10000000) in
      let c2 = char_of_int ((i          land 0b00111111) lor 0b10000000) in
      let s = Bytes.create 3 in
      Bytes.set s 0 c0; Bytes.set s 1 c1; Bytes.set s 2 c2;
      Bytes.to_string s
    else if i <= 0x10FFFF then
      let c0 = char_of_int (((i lsr 18) land 0b00000111) lor 0b11110000) in
      let c1 = char_of_int (((i lsr 12) land 0b00111111) lor 0b10000000) in
      let c2 = char_of_int (((i lsr 6)  land 0b00111111) lor 0b10000000) in
      let c3 = char_of_int ((i          land 0b00111111) lor 0b10000000) in
      let s = Bytes.create 4 in
      Bytes.set s 0 c0; Bytes.set s 1 c1;
      Bytes.set s 2 c2; Bytes.set s 3 c3;
      Bytes.to_string s
    else
      raise (invalid_arg "UTF8.encode")

  (*
   * Decode a UTF8 character at a given position in a string.
   * Arguments:
   *   s : the string,
   *   i : index where to look.
   * Returns a couple (c, l) where c is the code of the character and l is the
   * number of bytes read.
   * Raise invalid_arg if s.[i] is not a valid first byte for a UTF8 character.
   * No checks are run on the hypothetical second, third and fourth byte (i.e.
   * length of s is not checked, and shape 0x10xxxxxx of byte is not checked).
   *)
  let decode : string -> int -> (Uchar.t * int) = fun s i ->
    let cc = Char.code s.[i] in
    if cc lsr 7 = 0 then
      (Uchar.of_int cc, 1)
    else if (cc lsr 6) land 1 = 0 then
      raise (invalid_arg "UTF8.decode")
    else if (cc lsr 5) land 1 = 0 then
      let i0 = (cc land 0b00011111) lsl 6 in
      let i1 = (Char.code s.[i+1]) land 0b00111111 in
      (Uchar.of_int (i0 lor i1), 2)
    else if (cc lsr 4) land 1 = 0 then
      let i0 = (cc land 0b00001111) lsl 12 in
      let i1 = ((Char.code s.[i+1]) land 0b00111111) lsl 6 in
      let i2 = (Char.code s.[i+2])  land 0b00111111 in
      (Uchar.of_int (i0 lor i1 lor i2), 3)
    else if (cc lsr 3) land 1 = 0 then
      let i0 = (cc land 0b00000111) lsl 18 in
      let i1 = ((Char.code s.[i+1]) land 0b00111111) lsl 12 in
      let i2 = ((Char.code s.[i+2]) land 0b00111111) lsl 6 in
      let i3 = (Char.code s.[i+3])  land 0b00111111 in
      (Uchar.of_int (i0 lor i1 lor i2 lor i3), 4)
    else
      raise (invalid_arg "UTF8.decode")

  let look : string -> int -> Uchar.t = fun s i ->
    fst (decode s i)

  let next : string -> int -> int = fun s i ->
    let (_, sz) = decode s i in
    i + sz

  let prev : string -> int -> int = fun s i ->
    let ps = List.filter (fun i -> i >= 0) [i-1; i-2; i-3; i-4] in
    let rec try_until_found l =
      match l with
      | []    -> assert false
      | p::ps -> try
                 let (_, sz) = decode s p in
                 if p + sz <> i then assert false;
                 p
               with
                 Invalid_argument _ -> try_until_found ps
    in try_until_found ps

  let fold : ('a -> Uchar.t -> 'a) -> 'a -> string -> 'a = fun f ini s ->
    if s = "" then
      ini
    else
      let l = String.length s in
      let rec fold' acc i =
      if i > l then
        raise (invalid_arg "UTF.fold")
      else if i = l then
        acc
      else
        let (u, l) = decode s i in
        fold' (f acc u) (i + l)
      in fold' ini 0

  let of_list : Uchar.t list -> string = fun l ->
    String.concat "" (List.map encode l)

  let to_list : string -> Uchar.t list = fun s ->
    List.rev (fold (fun acc x -> x::acc) [] s)

  let grapheme_break : string -> int -> bool = fun s pos ->
    let rec previous_ri acc i0 bp1 =
      match bp1 with
      | RegionalIndicator ->
         begin
           if i0 = 0 then not acc else
             let i0 = prev s i0 in
             let c1 = look s i0 in
             let bp1 = gbp c1 in
             previous_ri (not acc) i0 bp1
         end
      | _ -> acc
    in
    let rec previous_pict i0 bp1 =
      match bp1 with
      | Extend ->
         begin
           if i0 = 0 then false else
             let i0 = prev s i0 in
             let c1 = look s i0 in
             let bp1 = gbp c1 in
             previous_pict i0 bp1
         end
      | ExtPict -> true
      | _       -> false
    in
    if pos = 0 || pos >= String.length s then true else
      let i0 = prev s pos in
      let c1 = look s i0 and c2 = look s pos in
      let bp1 = gbp c1 and bp2 = gbp c2 in
      let previous =
        match bp1 with
        | ZWJ ->
           if previous_pict i0 Extend then ExtPictExtendStar
           else NoPrevious
        | RegionalIndicator ->
           if previous_ri false i0 RegionalIndicator then EvenRegionalIndicator
           else NoPrevious
        | _ -> NoPrevious
      in
      break_between previous bp1 bp2

  let grapheme_break_to_string = function
    | CR -> "CR"
    | LF -> "LF"
    | Control -> "Control"
    | SpacingMark -> "SpacingMark"
    | Extend   -> "Extend"
    | ZWJ -> "ZWJ"
    | Prepend -> "Prepend"
    | ExtPict -> "ExtPict"
    | RegionalIndicator -> "RegionalIndicator"
    | L -> "L" | V -> "V" | T -> "T" | LV -> "LV" | LVT -> "LVT"
    | Other         -> "Other"

  let grapheme_break_after : Uchar.t list -> Uchar.t -> bool = fun s c2 ->
    let rec previous_ri s acc bp1 =
      match bp1, s with
      | RegionalIndicator, [] -> not acc
      | RegionalIndicator, (c1::s) ->
         begin
           let bp1 = gbp c1 in
           previous_ri s (not acc) bp1
         end
      | _ -> acc
    in
    let rec previous_pict s bp1 =
      match bp1, s with
      | ExtPict, _ -> true
      | Extend, (c1::s) ->
         begin
           let bp1 = gbp c1 in
           previous_pict s bp1
         end
      | _       -> false
    in
    match s with
    | [] -> true
    | (c1::s) ->
       let bp1 = gbp c1 and bp2 = gbp c2 in
       Printf.eprintf "%x %s %d %s\n%!" (Uchar.to_int c1) (grapheme_break_to_string bp1)
         (Uchar.to_int c2) (grapheme_break_to_string bp2);
       let previous =
         match bp1 with
         | ZWJ ->
            if previous_pict s Extend then ExtPictExtendStar
            else NoPrevious
         | RegionalIndicator ->
            if previous_ri s false RegionalIndicator then EvenRegionalIndicator
            else NoPrevious
         | _ -> NoPrevious
       in
       break_between previous bp1 bp2

  let next_grapheme : string -> int -> int = fun s pos ->
    let npos = ref pos in
    try
      while !npos < String.length s do
        npos := next s !npos;
        if grapheme_break s !npos then raise Exit
      done;
      raise Not_found (* end of string *)
    with
      Exit -> !npos

  let prev_grapheme : string -> int -> int = fun s pos ->
    let npos = ref pos in
    try
      while !npos > 0 do
        npos := prev s !npos;
        if grapheme_break s !npos then raise Exit
      done;
      0
    with
      Exit -> !npos

  let fold_grapheme : (string -> 'a -> 'a) -> 'a -> string -> 'a =
    fun fn acc s ->
    let pos = ref 0 in
    let res = ref acc in
    while !pos < String.length s do
      let npos = next_grapheme s !pos in
      let s = String.sub s !pos (npos - !pos) in
      pos := npos;
      res := fn s !res;
    done;
    !res
]

let _ = Pparse.(write_ast Structure outfile str_items)
