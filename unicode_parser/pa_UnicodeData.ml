open Pacomb
open UTFTypes
open Grammar

let general_category_of_string = function
  | "Lu" -> Lu | "Ll" -> Ll | "Lt" -> Lt | "Mn" -> Mn | "Mc" -> Mc
  | "Me" -> Me | "Nd" -> Nd | "Nl" -> Nl | "No" -> No | "Zs" -> Zs
  | "Zl" -> Zl | "Zp" -> Zp | "Cc" -> Cc | "Cf" -> Cf | "Cs" -> Cs
  | "Co" -> Co | "Cn" -> Cn
  | "Lm" -> Lm | "Lo" -> Lo | "Pc" -> Pc | "Pd" -> Pd | "Ps" -> Ps
  | "Pe" -> Pe | "Pi" -> Pi | "Pf" -> Pf | "Po" -> Po | "Sm" -> Sm
  | "Sc" -> Sc | "Sk" -> Sk | "So" -> So
  | s -> Lex.give_up ~msg:(Printf.sprintf "Missing: %s\n%!" s) ()

let combining_class_of_int = function
  | n when n >= 10 && n <= 199
        -> Fixed_position n
  | 0   -> Spacing_split_enclosing_reordrant_and_Tibetan_subjoined
  | 1   -> Overlays_and_interior
  | 7   -> Nuktas
  | 8   -> Hiragana_Katakana_voicing_marks
  | 9   -> Viramas
  | 200 -> Below_left_attached
  | 202 -> Below_attached
  | 204 -> Below_right_attached
  | 208 -> Left_attached
  | 210 -> Right_attached
  | 212 -> Above_left_attached
  | 214 -> Above_attached
  | 216 -> Above_right_attached
  | 218 -> Below_left
  | 220 -> Below
  | 222 -> Below_right
  | 224 -> Left
  | 226 -> Right
  | 228 -> Above_left
  | 230 -> Above
  | 232 -> Above_right
  | 233 -> Double_below
  | 234 -> Double_above
  | 240 -> Below_iota_subscript
  | i -> Lex.give_up ~msg:(Printf.sprintf "Missing: %i\n%!" i) ()


let bidirectional_mapping_of_string = function
  | "L"   -> L
  | "LRE" -> LRE
  | "LRO" -> LRO
  | "LRI" -> LRI
  | "R"   -> R
  | "AL"  -> AL
  | "RLE" -> RLE
  | "RLO" -> RLO
  | "RLI" -> RLI
  | "PDF" -> PDF
  | "PDI" -> PDI
  | "FSI" -> FSI
  | "EN"  -> EN
  | "ES"  -> ES
  | "ET"  -> ET
  | "AN"  -> AN
  | "CS"  -> CS
  | "NSM" -> NSM
  | "BN"  -> BN
  | "B"   -> B
  | "S"   -> S
  | "WS"  -> WS
  | "ON"  -> ON
  | s -> Lex.give_up ~msg:(Printf.sprintf "Missing: %s\n%!" s) ()

let decomposition_tag_of_string = function
  | "font"     -> Font
  | "noBreak"  -> NoBreak
  | "initial"  -> Initial
  | "medial"   -> Medial
  | "final"    -> Final
  | "isolated" -> Isolated
  | "circle"   -> Circle
  | "super"    -> Super
  | "sub"      -> Sub
  | "vertical" -> Vertical
  | "wide"     -> Wide
  | "narrow"   -> Narrow
  | "small"    -> Small
  | "square"   -> Square
  | "fraction" -> Fraction
  | "compat"   -> Compat
  | s -> Lex.give_up ~msg:(Printf.sprintf "Missing: %s\n%!" s) ()

let%parser code =
  (c::RE"[0-9A-F]+") => Uchar.unsafe_of_int (int_of_string ("0x" ^ c))

let%parser integer =
  (c::RE"[-+]?[0-9]+") => int_of_string c

let%parser integer64 =
  (c::RE"[-+]?[0-9]+") => Int64.of_string c

let%parser fraction =
    (n::integer64) '/' (d::integer) => (n,d)
  ; (n::integer64) => (n,1)

let%parser category =
  (c::RE"[A-Z][a-z]") => general_category_of_string c

let%parser bidirectional_mapping =
  (c::RE"[A-Z]+") => bidirectional_mapping_of_string c

let%parser combining_class =
  (c::RE("[0-9]+")) => combining_class_of_int (int_of_string c)

let%parser mirrored =
    'Y' => true
  ; 'N' => false

let%parser decomposition =
  let decomposition_tag =
      '<' (t::RE"[a-zA-Z]+") '>' => decomposition_tag_of_string t
  in (t::default_option Canonical decomposition_tag) (cs::star code) => (t,cs)

let%parser name =
    (n::RE"[-()A-Za-z0-9]+") => n
  ; "<control>"              => "<control>"
(*  | '(' n:"<control>" ')' *)

let%parser old_name =
     () => ""
   ; (n::RE"[-A-Za-z0-9 ()]+") => n

type kind = Single of Uchar.t * char_description
          | Range  of Uchar.t * Uchar.t * (Uchar.t -> char_description)

let%parser single =
    (code::code) ';'
    (name::plus name) ';'
    (gen_cat::category) ';'
    (c_cl::combining_class) ';'
    (bid_map::bidirectional_mapping) ';'
    (dec::decomposition) ';'
    (decimal::option integer) ';'
    (digit::option integer) ';'
    (numeric::option fraction) ';'
    (mirrored::mirrored) ';'
    (oldName::old_name) ';'
    (comments::(() => "" ; (c::RE"[^;\n]+") => c)) ';'
    (uppercase::option code) ';'
    (lowercase::option code) ';'
    (titlecase::option code) '\n' =>
      let desc =
        { code                  = code
        ; name                  = name
        ; general_category      = gen_cat
        ; combining_class       = c_cl
        ; bidirectional_mapping = bid_map
        ; decomposition         = dec
        ; decimal_digit_value   = decimal
        ; digit_value           = digit
        ; numeric_value         = numeric
        ; mirrored              = mirrored
        ; oldName               = oldName
        ; comments              = comments
        ; uppercase             = uppercase
        ; lowercase             = lowercase
        ; titlecase             = titlecase
        }
      in Single (code, desc)

let%parser range =
    (firstcode::code) ';'
    '<' (gname::plus name) ',' "First" '>' ';'
    (gen_cat::category) ';'
    (c_cl::combining_class) ';'
    (bid_map::bidirectional_mapping) ';'
    (dec::decomposition) ';'
    (decimal::option integer) ';'
    (digit::option integer) ';'
    (numeric::option fraction) ';'
    (mirrored::mirrored) ';'
    (oldName::old_name) ';'
    (comments::(() => "" ; (c::RE"[^;\n]+") => c)) ';'
    (uppercase::option code) ';'
    (lowercase::option code) ';'
    (titlecase::option code) '\n'
    (lastcode::code) ';'
    '<' (plus name) ", Last>" ';'
    category ';'
    combining_class ';'
    bidirectional_mapping ';'
    decomposition ';'
    (option integer) ';'
    (option integer) ';'
    (option fraction) ';'
    mirrored ';'
    old_name ';'
    (() => "" ; (c::RE"[^;\n]+") => c) ';'
    (option code) ';'
    (option code) ';'
    (option code) '\n' =>
      let build_desc c =
        if c < firstcode || c > lastcode then assert false;
        { code                  = c
        ; name                  = gname
        ; general_category      = gen_cat
        ; combining_class       = c_cl
        ; bidirectional_mapping = bid_map
        ; decomposition         = dec
        ; decimal_digit_value   = decimal
        ; digit_value           = digit
        ; numeric_value         = numeric
        ; mirrored              = mirrored
        ; oldName               = oldName
        ; comments              = comments
        ; uppercase             = uppercase
        ; lowercase             = lowercase
        ; titlecase             = titlecase
        }
      in Range (firstcode, lastcode, build_desc)

let%parser file_contents =
  (l::star ( (s::single) => s
           ; (r::range) => r)) => l

let blank = Lex.blank_regexp "[ \t\r]*"
let parse = parse_channel file_contents blank

let flatten_data ld =
  let rec add_data acc ld =
    match ld with
    | []                   -> List.rev acc
    | Single (k,v) :: ls   -> add_data ((Uchar.to_int k,v)::acc) ls
    | Range (f,l,bf) :: ls ->
        if f > l then add_data acc ls
        else add_data ((Uchar.to_int f, bf f)::acc) (Range (Uchar.succ f, l, bf) :: ls)
  in
  add_data [] ld

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

  let data = flatten_data ld in

  (* Adding the data to the permanent map *)
  PermanentMap.new_map outfile; (* Fails if file exists *)
  let m = PermanentMap.open_map outfile in
  PermanentMap.add_many m data;

  (* Compacting *)
  PermanentMap.compact m;
  PermanentMap.close_map m
