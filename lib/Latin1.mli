(** {1 convert Latin1 ISO/IEC 8859-1 encoding to unicode} *)

val to_uchar : char -> Uchar.t
val to_utf8 : string -> string
val to_utf16 : string -> string
val to_utf32 : string -> string
