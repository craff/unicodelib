open Pacomb
open Grammar
open Unicodelib

(* Blank function *)
let blank = Lex.blank_regexp "\\(\\([#@][^\n]*\\)\\|[ \r\t\026]+\\)*"
   (* bug: "\\([ \r\t\026]\\|\\(\\(#[^\n]*\\)\\)*" *)

(* Parser for hexadecimal integers *)
let%parser char = (i::RE"[0-9a-fA-F]+") => Uchar.of_int (int_of_string ("0x" ^ i))

let%parser string = (l::star char) => UTF8.of_list l

let good = ref true

let print ch s =
  let l = UTF8.to_list s in
  List.iter (fun x -> Printf.fprintf ch "%04x " (Uchar.to_int x)) l

let test pos x _nfc nfd _nfkc nfkd =
  let nfd' = UTF8.nfd x in
  if nfd' <> nfd then
    begin
      good := false;
      Printf.eprintf "line %d, bad nfd %a => %a <> %a\n%!" pos.Pos.line print x print nfd' print nfd;
    end;
  let nfkd' = UTF8.nfkd x in
  if nfkd' <> nfkd then
    begin
      good := false;
      Printf.eprintf "line %d, bad nfkd %a => %a <> %a\n%!" pos.Pos.line print x print nfkd' print nfkd
    end

  (* Single mapping parser *)
let%parser test =
  (x::string) ';' (nfc::string) ';' (nfd::string)
              ';' (nfkc::string) ';' (nfkd::string) ';' (plus ('\n' => ())) =>
    test x_lpos x nfc nfd nfkc nfkd

let%parser tests =
  (star ('\n' => ())) (star test) => ()

let parse = parse_channel tests blank

let _ =
  (* Command line args *)
  if Array.length Sys.argv != 2 then
    begin
      let pn = Sys.argv.(0) in
      Printf.eprintf "Usage: %s <NormalizationTest.txt>" pn;
      exit 1
    end;
  let infile = Sys.argv.(1) in

  (* Parsing and preparing the data *)
  let infile = open_in infile in
  let _ = Pos.handle_exception parse infile in

  close_in infile;
  if not !good then exit 1
