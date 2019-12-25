

module Make (A : sig val conversion_array : int array end) =
  struct
    open A

    exception Undefined

    let to_uchar : char -> Uchar.t =
      fun c ->
      let i = Char.code c in
      if (i < 0) || (i > 255) then raise Undefined;
      let u = conversion_array.(i) in
      if u < 0 then raise Undefined; Uchar.of_int u

    let to_utf8 : string -> string =
      fun s -> UTF8.init (String.length s)
                 (fun i -> to_uchar (s.[i - 1]))

    let to_utf16 : string -> string =
      fun s -> UTF16.init (String.length s)
                 (fun i -> to_uchar (s.[i - 1]))

    let to_utf32 : string -> string =
      fun s -> UTF32.init (String.length s)
                 (fun i -> to_uchar (s.[i - 1]))
  end
