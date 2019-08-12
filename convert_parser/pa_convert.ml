open Pacomb
open Grammar

(* Blank function *)
let blank = Lex.blank_regexp "\\(\\(#[^\n]*\\)\\|[ \r\t\026]+\\)*"
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

  let outfile = open_out outfile in
  Printf.fprintf outfile "let conversion_array : int array =\n";
  Printf.fprintf outfile "  let arr = Array.make 255 (-1) in\n";
  List.iter (fun (i,j) ->
      Printf.fprintf outfile
                    "  arr.(%d) <- %d;\n" i j) ld;
  Printf.fprintf outfile "  arr\n%!";
  Printf.fprintf outfile "\n\
     exception Undefined\n\
     \n\
     let to_uchar : char -> Uchar.t =\n\
       fun c ->\n\
         let i = Char.code c in\n\
         if (i < 0) || (i > 255) then raise Undefined;\n\
                 (let u = conversion_array.(i) in if u < 0 then raise Undefined; Uchar.of_int u)\n\
     \n\
     let to_utf8 : string -> string =\n\
       fun s -> UTF8.init (String.length s) (fun i -> to_uchar (s.[i - 1]))\n\
     let to_utf16 : string -> string =\n\
       fun s -> UTF16.init (String.length s) (fun i -> to_uchar (s.[i - 1]))\n\
     let to_utf32 : string -> string =\n\
       fun s -> UTF32.init (String.length s) (fun i -> to_uchar (s.[i - 1]))\n\
                          ";
  close_out outfile
