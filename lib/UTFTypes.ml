exception Out_of_bound

type general_category =
  (* normative *)
  | Lu | Ll | Lt | Mn | Mc | Me | Nd | Nl | No
  | Zs | Zl | Zp | Cc | Cf | Cs | Co | Cn
  (* informative *)
  | Lm | Lo | Pc | Pd | Ps | Pe | Pi | Pf | Po
  | Sm | Sc | Sk | So

type combining_class =
  | Starter
  | Overlays_and_interior
  | Nuktas
  | Hiragana_Katakana_voicing_marks
  | Viramas
  | Fixed_position of int
  | Below_left_attached
  | Below_attached
  | Below_right_attached
  | Left_attached
  | Right_attached
  | Above_left_attached
  | Above_attached
  | Above_right_attached
  | Below_left
  | Below
  | Below_right
  | Left
  | Right
  | Above_left
  | Above
  | Above_right
  | Double_below
  | Double_above
  | Below_iota_subscript

type bidirectional_mapping =
  | L   (* Left-to-Right *)
  | LRE (* Left-to-Right Embedding *)
  | LRO (* Left-to-Right Override *)
  | LRI (* Left-to-Right Isolate *)
  | R   (* Right-to-Left *)
  | AL  (* Right-to-Left Arabic *)
  | RLE (* Right-to-Left Embedding *)
  | RLO (* Right-to-Left Override *)
  | RLI (* Right-to-Left Isolate *)
  | PDF (* Pop Directional Format *)
  | PDI (* Pop Directional Isolate *)
  | FSI (* ??? Isolate *)
  | EN  (* European Number *)
  | ES  (* European Number Separator *)
  | ET  (* European Number Terminator *)
  | AN  (* Arabic Number *)
  | CS  (* Common Number Separator *)
  | NSM (* Non-Spacing Mark *)
  | BN  (* Boundary Neutral *)
  | B   (* Paragraph Separator *)
  | S   (* Segment Separator *)
  | WS  (* Whitespace *)
  | ON  (* Other Neutral *)

type decomposition_tag =
  | Font        (* A font variant (e.g. a blackletter form). *)
  | NoBreak     (* A no-break version of a space or hyphen. *)
  | Initial     (* An initial presentation form (Arabic). *)
  | Medial      (* A medial presentation form (Arabic). *)
  | Final       (* A final presentation form (Arabic). *)
  | Isolated    (* An isolated presentation form (Arabic). *)
  | Circle      (* An encircled form. *)
  | Super       (* A superscript form. *)
  | Sub         (* A subscript form. *)
  | Vertical    (* A vertical layout presentation form. *)
  | Wide        (* A wide (or zenkaku) compatibility character. *)
  | Narrow      (* A narrow (or hankaku) compatibility character. *)
  | Small       (* A small variant form (CNS compatibility). *)
  | Square      (* A CJK squared font variant. *)
  | Fraction    (* A vulgar fraction form. *)
  | Compat      (* Otherwise unspecified compatibility character. *)
  | Canonical

type char_description =
  { code                  : Uchar.t
  ; name                  : string list
  ; general_category      : general_category
  ; combining_class       : combining_class
  ; bidirectional_mapping : bidirectional_mapping
  ; decomposition         : (decomposition_tag * Uchar.t list) option
  ; composition_exclusion : bool
  ; decimal_digit_value   : int option
  ; digit_value           : int option
  ; numeric_value         : (int64 * int) option
  ; mirrored              : bool
  ; oldName               : string
  ; comments              : string
  ; uppercase             : Uchar.t option
  ; lowercase             : Uchar.t option
  ; titlecase             : Uchar.t option
  }

let combining_class_to_int = function
  | Fixed_position n -> n
  | Starter -> 0
  | Overlays_and_interior   -> 1
  | Nuktas   -> 7
  | Hiragana_Katakana_voicing_marks   -> 8
  | Viramas   -> 9
  | Below_left_attached -> 200
  | Below_attached -> 202
  | Below_right_attached -> 204
  | Left_attached -> 208
  | Right_attached -> 210
  | Above_left_attached -> 212
  | Above_attached -> 214
  | Above_right_attached -> 216
  | Below_left -> 218
  | Below -> 220
  | Below_right -> 222
  | Left -> 224
  | Right -> 226
  | Above_left -> 228
  | Above -> 230
  | Above_right -> 232
  | Double_below -> 233
  | Double_above -> 234
  | Below_iota_subscript -> 240

type prefix_tree = Node of (Uchar.t, Uchar.t option * prefix_tree) Hashtbl.t
