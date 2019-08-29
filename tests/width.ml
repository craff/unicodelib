open Unicodelib

let tbl1 = ref []
let tbl2 = ref []

let _ =
  let c = ref Uchar.min in

  try while true do
    let w = CharInfo.width !c in
    Printf.printf "\r%x/%x => %d    %!"
      (Uchar.to_int !c) (Uchar.to_int Uchar.max) w;
    begin
      match !tbl1 with
      | ((x,_),w')::l when w = w' -> tbl1 := ((x,!c),w')::l
      | _                         -> tbl1 := ((!c,!c),w)::!tbl1
    end;
    let w = CharInfo.width ~context:EastAsian !c in
    Printf.printf "\r%x/%x => %d    %!"
      (Uchar.to_int !c) (Uchar.to_int Uchar.max) w;
    begin
      match !tbl2 with
      | ((x,_),w')::l when w = w' -> tbl2 := ((x,!c),w')::l
      | _                         -> tbl2 := ((!c,!c),w)::!tbl2
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

let rec expr : ((Uchar.t * Uchar.t) * int) list -> expression = fun l -> match l with
    | [] -> assert false
    | [(_,w)] -> Exp.constant (Const.int w)
    | l ->
       let (l1,l2) = split l in
       let x = match l2 with
         | [] -> assert false
         | ((x,_),_)::_ -> x
       in
       [%expr if n < [%e Exp.constant (Const.int (Uchar.to_int x))] then
             [%e expr l1] else [%e expr l2]]

let str_items = [%str
                 type context = CJK | Other
                 let width ?(context=Other) c =
                   let n = Uchar.to_int c in
                   if context = CJK then
                     [%e expr !tbl2]
                   else
                     [%e expr !tbl1]]

let outfile = Sys.argv.(1)

let _ = Pparse.(write_ast Structure outfile str_items)
