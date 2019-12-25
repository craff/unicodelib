let mkloc = Location.mkloc
let mknoloc = Location.mknoloc
open Pacomb
open Grammar
open Asttypes
open Ast_helper
open Longident

(* Blank function *)
let blank = Regexp.blank_regexp "\\(\\(#[^\n]*\\)\\|[ \r\t\026]+\\)*"
   (* bug: "\\([ \r\t\026]\\|\\(\\(#[^\n]*\\)\\)*" *)

(* Parser for hexadecimal integers *)
let%parser ex_int = (i::RE"0x[0-9a-fA-F]+") => int_of_string i

(* Single mapping parser *)
let%parser mapping =
    (i::ex_int) (j::default_option (-1) ex_int) (RE"\n+") => (i,j)

let%parser mappings =
  (star ('\n' => ())) (ms::star mapping)  => ms

let parse = parse_channel mappings blank

let _ =
  (* Command line args *)
  if Array.length Sys.argv != 3 then
    begin
      let pn = Sys.argv.(0) in
      Printf.eprintf "Usage: %s <path_to_UnicodeData.txt> <output_file>" pn;
      exit 1
    end;
  let infile = Sys.argv.(1) in
  let outfile = Sys.argv.(2) in

  (* Parsing and preparing the data *)
  let infile = open_in infile in
  let ld = Pos.handle_exception parse infile in

  close_in infile;

  let int n = Exp.constant (Const.int n) in
  let id n = Exp.ident (mknoloc (Lident n)) in
  let mid m n = Exp.ident (mknoloc (Ldot(Lident m,n))) in
  let pid n = Pat.var (mknoloc n) in
  let seq = ref (id "a") in
  List.iter (fun (i,j) ->
      seq :=
        Exp.sequence
          (Exp.apply (mid "Array" "set")
             [ Nolabel, id "a"
             ; Nolabel, int i
             ; Nolabel, int j ])
          !seq) ld;
  let str_items =
    [Str.value Nonrecursive
       [Vb.mk (pid "conversion_array")
          (Exp.let_ Nonrecursive
             [Vb.mk (pid "a")
                (Exp.apply (mid "Array" "make")
                   [ Nolabel, int 255
                   ; Nolabel, int (-1)])]
             !seq)]
    ;Str.include_
       { pincl_mod =
           (Mod.apply
              (Mod.ident (mknoloc (Ldot(Lident "GenConvert","Make"))))
              (Mod.structure
                 [Str.value Nonrecursive
                    [Vb.mk (pid "conversion_array")
                       (id "conversion_array")]]))
       ; pincl_loc = Location.none
       ; pincl_attributes = [] }

    ]
  in
  Pparse.(write_ast Structure outfile str_items)
