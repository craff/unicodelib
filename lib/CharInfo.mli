(** {1 Types and functions to access unicode characters properties} *)

(** The general category: for instance Ll = lowercase letter *)
type general_category =
  (* normative *)
  | Lu | Ll | Lt | Mn | Mc | Me | Nd | Nl | No
  | Zs | Zl | Zp | Cc | Cf | Cs | Co | Cn
  (* informative *)
  | Lm | Lo | Pc | Pd | Ps | Pe | Pi | Pf | Po
  | Sm | Sc | Sk | So

(** The combining class: used by normalization to order characters *)
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

(** For ordering, this function is used:
   - 0 means do not change order.
   - if  both characters  have  non  zero combining  class,  order in  ascending
     order. *)
val combining_class_to_int : combining_class -> int

type bidirectional_mapping =
  | L   (** Left-to-Right *)
  | LRE (** Left-to-Right Embedding *)
  | LRO (** Left-to-Right Override *)
  | LRI (** Left-to-Right Isolate *)
  | R   (** Right-to-Left *)
  | AL  (** Right-to-Left Arabic *)
  | RLE (** Right-to-Left Embedding *)
  | RLO (** Right-to-Left Override *)
  | RLI (** Right-to-Left Isolate *)
  | PDF (** Pop Directional Format *)
  | PDI (** Pop Directional Isolate *)
  | FSI (** ??? Isolate *)
  | EN  (** European Number *)
  | ES  (** European Number Separator *)
  | ET  (** European Number Terminator *)
  | AN  (** Arabic Number *)
  | CS  (** Common Number Separator *)
  | NSM (** Non-Spacing Mark *)
  | BN  (** Boundary Neutral *)
  | B   (** Paragraph Separator *)
  | S   (** Segment Separator *)
  | WS  (** Whitespace *)
  | ON  (** Other Neutral *)

(** The decomposition tag giving the "meaning" of the decomposition of a unicode
    character *)
type decomposition_tag =
  | Font        (** A font variant (e.g. a blackletter form). *)
  | NoBreak     (** A no-break version of a space or hyphen. *)
  | Initial     (** An initial presentation form (Arabic). *)
  | Medial      (** A medial presentation form (Arabic). *)
  | Final       (** A final presentation form (Arabic). *)
  | Isolated    (** An isolated presentation form (Arabic). *)
  | Circle      (** An encircled form. *)
  | Super       (** A superscript form. *)
  | Sub         (** A subscript form. *)
  | Vertical    (** A vertical layout presentation form. *)
  | Wide        (** A wide (or zenkaku) compatibility character. *)
  | Narrow      (** A narrow (or hankaku) compatibility character. *)
  | Small       (** A small variant form (CNS compatibility). *)
  | Square      (** A CJK squared font variant. *)
  | Fraction    (** A vulgar fraction form. *)
  | Compat      (** Otherwise unspecified compatibility character. *)
  | Canonical   (** Fully equivalent character *)

(** The character description *)
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

(** Access to the character description.
    Note: the descriptions are stored in a sqlite3 data base and cached in memory
    so only the first access need to read in the database. *)
val get_char_descr : Uchar.t -> char_description

(** A few function to directly access some properties *)
val general_category : Uchar.t -> general_category
val unicode_name : Uchar.t -> string
val to_lower : Uchar.t -> Uchar.t option
val to_upper : Uchar.t -> Uchar.t option
val is_space : Uchar.t -> bool

(** private type and function used by normalization *)
type prefix_tree = Node of (Uchar.t, Uchar.t option * prefix_tree) Hashtbl.t
val prefix_tree : prefix_tree Lazy.t
