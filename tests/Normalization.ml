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

let test pos x nfc nfd nfkc nfkd =
  let cmp name s1 s2 =
    if s1 <> s2 then
      begin
        good := false;
        Printf.eprintf "line %d, bad %s %a => %a <> %a\n%!"
          pos.Pos.line name print x print s1 print s2;
      end
  in
  cmp "nfd" (UTF8.nfd x) nfd;
  cmp "nfc" (UTF8.nfc x) nfc;
  cmp "nfkd" (UTF8.nfkd x) nfkd;
  cmp "nfkc" (UTF8.nfkc x) nfkc

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
