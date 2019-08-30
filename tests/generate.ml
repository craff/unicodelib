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
    Printf.printf "\r%x/%x => %d    %!"
      (Uchar.to_int !c) (Uchar.to_int Uchar.max) w;
    begin
      match !tbl1 with
      | ((x,_),w')::l when w = w' -> tbl1 := ((x,!c),w')::l
      | _                         -> tbl1 := ((!c,!c),w)::!tbl1
    end;
    let w = width ~context:EastAsian !c in
    Printf.printf "\r%x/%x => %d    %!"
      (Uchar.to_int !c) (Uchar.to_int Uchar.max) w;
    begin
      match !tbl2 with
      | ((x,_),w')::l when w = w' -> tbl2 := ((x,!c),w')::l
      | _                         -> tbl2 := ((!c,!c),w)::!tbl2
    end;
    let gb = try (get_char_descr !c).grapheme_break
             with Not_found -> Other
    in
    Printf.printf "\r%x/%x => %s    %!"
      (Uchar.to_int !c) (Uchar.to_int Uchar.max) (grapheme_break_to_string gb);
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
         | ((x,_),_)::_ -> x
       in
       [%expr if n < [%e Exp.constant (Const.int (Uchar.to_int x))] then
             [%e expr fn l1] else [%e expr fn l2]]

let exp_from_int w = Exp.constant (Const.int w)
let exp_cname s = Exp.construct (Location.mknoloc (Lident (CharInfo.grapheme_break_to_string s))) None

let str_items = [%str
  type context = ASCII | UTF8 | CJK_UTF8
  let width ?(context=UTF8) c =
    let n = Uchar.to_int c in
    if context = CJK then
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
]

let _ = Pparse.(write_ast Structure outfile str_items)
