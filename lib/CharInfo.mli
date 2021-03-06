(** {1 Types and functions to access unicode characters properties} *)

(** The general category: for instance Ll = lowercase letter *)
type general_category =
  (* normative *)
  | Lu (* Letter, Uppercase *)
  | Ll (* Letter, Lowercase *)
  | Lt (* Letter, Titlecase *)
  | Mn (* Mark, non spacing *)
  | Mc (* Mark, combining *)
  | Me (* Mark, enclosing *)
  | Nd (* Number, decimal *)
  | Nl (* Number, letter *)
  | No (* Number, other *)
  | Zs (* Separator, space *)
  | Zl (* Separator, line *)
  | Zp (* Separator, paragraph *)
  | Cc (* Control, char *)
  | Cf (* Control, format *)
  | Cs (* Control, surrogate *)
  | Co (* Control, other *)
  | Cn (* Control, not assigned *)
  (* informative *)
  | Lm (* Letter, modifier *)
  | Lo (* Letter, other *)
  | Pc (* Punctuation, connector *)
  | Pd (* Punctuation, dash *)
  | Ps (* Punctuation, open *)
  | Pe (* Punctuation, close *)
  | Pi (* Punctuation, initial quote *)
  | Pf (* Punctuation, final quote *)
  | Po (* Punctuation, other *)
  | Sm (* Symbol, math *)
  | Sc (* Symbol, currency *)
  | Sk (* Symbol, modifier *)
  | So (* Symbol, other *)

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

type east_asian_width_category =
  | Neutral
  | Narrow
  | FullWidth
  | HalfWidth
  | Wide
  | Ambiguous

type grapheme_break_property =
  | Other
  | CR
  | LF
  | Prepend
  | Control
  | Extend
  | SpacingMark
  | L | V | T | LV | LVT
  | ZWJ
  | RegionalIndicator
  | ExtPict

val grapheme_break_to_string : grapheme_break_property -> string

type emoji =
  | NotEmoji
  | Emoji
  | EmojiPresentation
  | EmojiModifier
  | EmojiModifierBase
  | EmojiComponent
  | ExtendedPictographic

(** The character description *)
type char_description =
  { code                  : Uchar.t
  ; name                  : string list
  ; general_category      : general_category
  ; combining_class       : combining_class
  ; bidirectional_mapping : bidirectional_mapping
  ; decomposition         : (decomposition_tag * Uchar.t list) option
  ; composition_exclusion : bool
  ; east_asian_width      : east_asian_width_category
  ; grapheme_break        : grapheme_break_property
  ; emoji_type            : emoji
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

(** type for the block of a character.
    Variant type with only constant constructor taken from
    UNIDATA/Block.txt with the rule:

    ' ' => '_'
    '-' => '_'

    for instance "Kana Extended-A" gives "Kana_Extended_A" *)
type block = Blocks.block

val block : Uchar.t -> block


(** private type and function used by normalization *)
type prefix_tree = Node of (Uchar.t, Uchar.t option * prefix_tree) Hashtbl.t
val prefix_tree : prefix_tree Lazy.t

type width_context = EastAsian | Other
(** Gives the column width according to

  https://stackoverflow.com/questions/3634627/how-to-know-the-preferred-display-width-in-columns-of-unicode-characters
  http://www.unicode.org/Public/UCD/latest/ucd/EastAsianWidth.txt
  https://www.unicode.org/reports/tr11
 *)
val width : ?context:width_context -> Uchar.t -> int

type previous_chars =
  EvenRegionalIndicator | ExtPictExtendStar | NoPrevious

(* [break_between previous c1 c2] returns true if and only if a grapheme break
   is between c1 and c2.
   previous must tel if there is an even number of RI before c1, if
   c1 is a RI. It should tell if there is a pattern
   ExtPict Extend* before c1 if c1 is ZWJ *)

val break_between : previous_chars -> grapheme_break_property ->
                    grapheme_break_property -> bool
