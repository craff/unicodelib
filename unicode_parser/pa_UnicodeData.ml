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
  | 0   -> Starter
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
  | "L"   -> (L:bidirectional_mapping)
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

let decomposition_tbl = Hashtbl.create 1024

let%parser code =
  (c::RE"[0-9A-F]+") => Uchar.unsafe_of_int (int_of_string ("0x" ^ c))

module NTest = struct
  (* Blank function *)
  let blank = Regexp.blank_regexp "\\(\\([#@][^\n]*\\)\\|[ \r\t\026]+\\)*"
  (* bug: "\\([ \r\t\026]\\|\\(\\(#[^\n]*\\)\\)*" *)

  let%parser string = (l::plus code) => l

  (* Single mapping parser *)

  let%parser decomposition =
    (x::code) ';' (__::string) ';' (nfd::string)
      ';' (__::string) ';' (nfkd::string) ';' (plus ('\n' => ())) =>
      if nfd = nfkd then Hashtbl.add decomposition_tbl x nfd
  ; code (__::string) ';' (__::string) ';' (__::string)
      ';' (__::string) ';' (__::string) ';' (plus ('\n' => ())) =>
      ()

  let%parser decompositions =
    (star ('\n' => ())) (star decomposition) => ()

  let parse = parse_channel decompositions blank
end

let exclusion_tbl = Hashtbl.create 1024

module CExcl = struct
  (* Blank function *)
  let blank = Regexp.blank_regexp "\\(\\([#@][^\n]*\\)\\|[ \r\t\026]+\\)*"
  (* bug: "\\([ \r\t\026]\\|\\(\\(#[^\n]*\\)\\)*" *)

  let%parser string = (l::plus code) => l

  (* Single mapping parser *)

  let%parser exclusion =
    (x::code) (plus ('\n' => ())) => Hashtbl.add exclusion_tbl x ()

  let%parser exclusions =
    (star ('\n' => ())) (star exclusion) => ()

  let parse = parse_channel exclusions blank
end

module EastAsianWidth = struct
  let blank = Regexp.blank_regexp "\\(\\(#[^\n]*\\)\\|[ \r\t\n]+\\)*"

  let%parser range = (c1::code) ".." (c2::code) => (c1,c2)
                   ; (c1::code)                 => (c1,c1)

  let%parser category =
      "N"  => Neutral
    ; "Na" => Narrow
    ; "F"  => FullWidth
    ; "H"  => HalfWidth
    ; "W"  => Wide
    ; "A"  => Ambiguous

  let%parser item = (r::range) ';' (c::category) => (r,c)

  let%parser items = Grammar.star item

  let parse = Grammar.parse_channel items blank
end

module GraphemeBreak = struct
  let blank = Regexp.blank_regexp "\\(\\(#[^\n]*\\)\\|[ \r\t\n]+\\)*"

  let%parser range = (c1::code) ".." (c2::code) => (c1,c2)
                   ; (c1::code)                 => (c1,c1)

  let%parser category =
      "CR"          => CR
    ; "LF"          => LF
    ; "Prepend"     => Prepend
    ; "Control"     => Control
    ; "Extend"      => Extend
    ; "SpacingMark" => SpacingMark
    ; "L"           => L
    ; "V"           => V
    ; "T"           => T
    ; "LV"          => LV
    ; "LVT"         => LVT
    ; "ZWJ"         => ZWJ
    ; "Regional_Indicator" => RegionalIndicator

  let%parser item = (r::range) ';' (c::category) => (r,c)

  let%parser items = Grammar.star item

  let parse = Grammar.parse_channel items blank
end

module Emoji = struct
  let blank = Regexp.blank_regexp "\\(\\(#[^\n]*\\)\\|[ \r\t\n]+\\)*"

  let%parser range = (c1::code) ".." (c2::code) => (c1,c2)
                   ; (c1::code)                 => (c1,c1)

  let%parser category =
      "Emoji"                 => Emoji
    ; "Emoji_Presentation"    => EmojiPresentation
    ; "Emoji_Modifier"        => EmojiModifier
    ; "Emoji_Modifier_Base"   => EmojiModifierBase
    ; "Emoji_Component"       => EmojiComponent
    ; "Extended_Pictographic" => ExtendedPictographic

  let%parser item = (r::range) ';' (c::category) => (r,c)

  let%parser items = Grammar.star item

  let parse = Grammar.parse_channel items blank
end

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
  in (t::default_option Canonical decomposition_tag) (cs::star code) =>
       if cs <> [] then Some(t,cs) else None

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
        ; composition_exclusion = false (* needs all characters to compute *)
        ; east_asian_width      = Neutral (* set later *)
        ; grapheme_break        = Other (* set later *)
        ; emoji_type            = NotEmoji  (* set later *)
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
        let get_dec dec = if dec = None then
                            try Some(Canonical,Hashtbl.find decomposition_tbl c)
                            with Not_found -> None
                          else dec
        in
        { code                  = c
        ; name                  = gname
        ; general_category      = gen_cat
        ; combining_class       = c_cl
        ; bidirectional_mapping = bid_map
        ; decomposition         = get_dec dec
        ; composition_exclusion = false (* set later *)
        ; east_asian_width      = Neutral (* set later *)
        ; grapheme_break        = Other (* set later *)
        ; emoji_type            = NotEmoji  (* set later *)
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

let blank = Regexp.blank_regexp "[ \t\r]*"
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

let set_composition_exclusion ld (k,c as r) =
  match c.decomposition with
  | Some(Canonical,l) ->
     let excl =
           List.length l = 1
       || (List.length l > 1 &&
            (c.combining_class <> Starter ||
               (List.assoc (Uchar.to_int (List.hd l)) ld).combining_class <> Starter))
       || Hashtbl.mem exclusion_tbl c.code
     in
     if excl then (k, { c with composition_exclusion = true }) else r
  | _ -> r

let prefix_tree = Node (Hashtbl.create 1024) [@@unboxed]

let add_to_prefix_tree c =
  match c.decomposition with
  | Some(Canonical,l) when not c.composition_exclusion  ->
     let rec fn k tbl0 (r,Node tbl) = function
       | [] ->
         begin
           match r with
           | None -> ()
           | Some(c') ->
              Printf.eprintf "conflict for composition: %x %x\n%!"
                (Uchar.to_int c.code)
                (Uchar.to_int c');
              assert false;
         end;
         Hashtbl.replace tbl0 k (Some c.code,Node tbl)
       | k::l -> gn k tbl l
       and gn k tbl l =
         let node = try Hashtbl.find tbl k
                    with Not_found ->
                      let node = (None, Node (Hashtbl.create 8)) in
                      Hashtbl.add tbl k node;
                      node
         in
         fn k tbl node l
     in
     begin
       match l with
       | [] -> assert false
       | k::l -> let Node tbl0 = prefix_tree in gn k tbl0 l
     end
  | _ -> ()

let _ =
  (* Command line args *)
  if Array.length Sys.argv != 8 then
    begin
      let pn = Sys.argv.(0) in
      Printf.eprintf "Usage: %s <path_to_UnicodeData.txt> <path_to_NormalizationTest.txt> <path_to_CompositionExclusion.txt> <path_to_EastAsianWidth.txt> <path_GraphemeBreakProperty.txt> <path_to_emoji-data.txt> <output_file>" pn;
      exit 1
    end;
  let infile = Sys.argv.(1) in
  let infile2 = Sys.argv.(2) in
  let infile3 = Sys.argv.(3) in
  let infile4 = Sys.argv.(4) in
  let infile5 = Sys.argv.(5) in
  let infile6 = Sys.argv.(6) in
  let outfile = Sys.argv.(7) in

  (* Parsing and preparing the data *)
  let infile = open_in infile in
  let ld = Pos.handle_exception parse infile in
  close_in infile;

  let infile2 = open_in infile2 in
  let _ = Pos.handle_exception NTest.parse infile2 in
  close_in infile2;

  let infile3 = open_in infile3 in
  let _ = Pos.handle_exception CExcl.parse infile3 in
  close_in infile3;

  let infile4 = open_in infile4 in
  let l = Pos.handle_exception EastAsianWidth.parse infile4 in
  let l =
    let rec fn acc l =
      match acc, l with
      | ((x1,y1),c1)::acc, ((x2,y2),c2)::l when x2 = Uchar.succ y1 && c1 = c2
                -> fn (((x1,y2),c1)::acc) l
      | _, i::l -> fn (i::acc) l
      | _, []   -> List.rev acc
    in
    fn [] l
  in
  let set_east_asian_width (i,d) =
    let (_,c) = List.find (fun ((x,y),_) -> x <= d.code && d.code <= y) l in
    (i, { d with east_asian_width = c })
  in
  close_in infile4;

  let infile5 = open_in infile5 in
  let l = Pos.handle_exception GraphemeBreak.parse infile5 in
  let l = List.sort (fun ((x,_),_) ((y,_),_) -> compare x y) l in
  let l =
    let rec fn acc l =
      match acc, l with
      | ((x1,y1),c1)::acc, ((x2,y2),c2)::l when x2 = Uchar.succ y1 && c1 = c2
        -> fn (((x1,y2),c1)::acc) l
      | _, i::l -> fn (i::acc) l
      | _, []   -> List.rev acc
    in
    fn [] l
  in
  let set_grapheme_break (i,d) =
    let c =
      try snd (List.find (fun ((x,y),_) -> x <= d.code && d.code <= y) l)
      with Not_found -> Other
    in
    let c =
      if (d.emoji_type = ExtendedPictographic ||
           d.emoji_type = Emoji) && c = Other
      then ExtPict
      else c
    in
    (i, { d with grapheme_break = c })
  in
  close_in infile5;

  let infile6 = open_in infile6 in
  let l = Pos.handle_exception Emoji.parse infile6 in
  let l = List.sort (fun ((x,_),_) ((y,_),_) -> compare x y) l in
  let l =
    let rec fn acc l =
      match acc, l with
      | ((x1,y1),c1)::acc, ((x2,y2),c2)::l when x2 = Uchar.succ y1 && c1 = c2
        -> fn (((x1,y2),c1)::acc) l
      | _, i::l -> fn (i::acc) l
      | _, []   -> List.rev acc
    in
    fn [] l
  in
  let set_emoji_type (i,d) =
    let c =
      try snd (List.find (fun ((x,y),_) -> x <= d.code && d.code <= y) l)
      with Not_found -> NotEmoji
    in
    (i, { d with emoji_type = c })
  in
  close_in infile5;

  let data = flatten_data ld in
  let data = List.map (set_composition_exclusion data) data in
  let data = List.map set_east_asian_width data in
  let data = List.map set_emoji_type data in
  let data = List.map set_grapheme_break data in

  (* Adding the data to the permanent map *)
  PermanentMap.new_map outfile; (* Fails if file exists *)
  let m = PermanentMap.open_map outfile in
  PermanentMap.add_many m data;

  List.iter (fun (_,c) -> add_to_prefix_tree c) data;
  PermanentMap.add m (-1) prefix_tree;

  (* Compacting *)
  PermanentMap.compact m;
  PermanentMap.close_map m
