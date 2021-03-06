let mkloc = Location.mkloc
let mknoloc = Location.mknoloc
open Pacomb
open Ast_helper
open Longident
open Parsetree

let blank = Regexp.blank_regexp "\\(\\(#[^\n]*\\)\\|[ \r\t\n]+\\)*"

let%parser code =
  (c::RE"[0-9A-F]+") => int_of_string ("0x" ^ c)

let%parser range = (c1::code) ".." (c2::code) => (c1,c2)

let camlify =
  String.map (function ' ' -> '_' | '-' -> '_' | c -> c)

let%parser name = Grammar.layout Blank.none
  ((s::RE"[-0-9A-Za-z ]+") (RE"\n")  => camlify s)

let%parser item = (r::range) ';' (name::name) => (r,name)

let%parser items = Grammar.star item

let parse = Grammar.parse_channel items blank

let int n = Exp.constant (Const.int n)
let id n = Exp.ident (mknoloc (Lident n))
let mid m n = Exp.ident (mknoloc (Ldot(Lident m,n)))
let pid n = Pat.var (mknoloc n)

let _ =
  (* Command line args *)
  if Array.length Sys.argv != 3 then
    begin
      let pn = Sys.argv.(0) in
      Printf.eprintf "Usage: %s <path_to_Block.txt> <output_file>" pn;
      exit 1
    end;
  let infile = Sys.argv.(1) in
  let outfile = Sys.argv.(2) in

  (* Parsing and preparing the data *)
  let infile = open_in infile in
  let blocks = Pos.handle_exception parse infile in
  close_in infile;

  let declarations =
    List.map (fun (_, c) ->  Type.constructor (mknoloc c))  blocks
  in

  let kind = Ptype_variant declarations in

  let td = Str.type_ Nonrecursive [Type.mk ~kind (mknoloc "block")] in

  let split l =
    let n = List.length l in
    let rec fn acc (n:int) l =
      if Stdlib.(n =  0) then (List.rev acc, l) else
        match l with
        | [] -> assert false
        | x::l -> fn (x::acc) (n-1) l
    in
    fn [] ((n+1)/2) l
  in

  let rec expr : ((int * int) * string) list -> expression = fun l -> match l with
    | [] -> assert false
    | [(_,name)] -> Exp.construct (mknoloc (Lident name)) None
    | l ->
       let (l1,l2) = split l in
       let x = match l2 with
         | [] -> assert false
         | ((x,_),_)::_ -> x
       in
       let n = Exp.ident (mknoloc (Lident "n")) in
       let x =  Exp.constant (Const.int x) in
       let cond =
         Exp.apply (Exp.ident (mknoloc (Lident("<")))) [Nolabel, n; Nolabel, x]
       in
       Exp.ifthenelse cond (expr l1) (Some (expr l2))
  in

  let str_items =
    [ td
    ; Str.value Nonrecursive
        [Vb.mk (pid "block")
           (Exp.fun_ Nolabel None (pid "c")
              (Exp.let_ Nonrecursive
                 [Vb.mk (pid "n")
                    (Exp.apply
                       (mid "Uchar" "to_int")
                       [Nolabel,  id"c"])]
                 (expr blocks)))]
    ]
  in

  Ocaml_common.Pparse.(write_ast Structure outfile str_items)
