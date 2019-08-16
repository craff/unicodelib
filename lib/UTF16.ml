module UTF16String = UTF.Make(
  struct
    (*
     * Encode a unicode character into a UTF16 string.
     * Argument:
     *   i : the unicode character.
     * Returns a string of size either 2 or 4.
     * Raise invalid_arg if i is not in the U+0000..U+10FFFF range.
     *)
    let encode : Uchar.t -> string = fun u ->
      let u = Uchar.to_int u in
      if u < 0 || u > 0x10FFFF then
        raise (invalid_arg "UF16.encode")
      else if u < 0x10000 then
        let s = Bytes.create 2 in
        Bytes.set s 0 (char_of_int ((u lsr 8) land 0xFF));
        Bytes.set s 1 (char_of_int (u land 0xFF));
        Bytes.to_string s
      else
        let u' = u - 0x10000 in
        let w1 = ((u' lsr 10) land 0b1111111111) lor 0xD800 in
        let w2 = (u' land 0b1111111111) lor 0xDC00 in
        let s = Bytes.create 4 in
        Bytes.set s 0 (char_of_int ((w1 lsr 8) land 0xFF));
        Bytes.set s 1 (char_of_int (w1 land 0xFF));
        Bytes.set s 2 (char_of_int ((w2 lsr 8) land 0xFF));
        Bytes.set s 3 (char_of_int (w2 land 0xFF));
        Bytes.to_string s

    (*
     * Decode a UTF16 character at a given position in a string.
     * Arguments:
     *   s : the string,
     *   i : index where to look.
     * Returns a couple (c, l) where c is the code of the character and l is the
     * number of bytes read.
     * Raise invalid_arg if no valid UTF16 character starts at poisition i in s.
     *)
    let decode : string -> int -> (Uchar.t * int) = fun s i ->
      let l = String.length s in
      if i > l - 1 then
        raise (invalid_arg "UTF16.decode")
      else
        let w1 = ((Char.code s.[i]) lsl 16) land (Char.code s.[i+1]) in
        if w1 < 0xD800 || w1 > 0xDFFF then
          (Uchar.of_int w1, 2)
        else if w1 >= 0xD800 && w1 <= 0xD8FF then
          raise (invalid_arg "UTF16.decode")
        else if i > l - 3 then
          raise (invalid_arg "UTF16.decode")
        else
          let w2 = ((Char.code s.[i+2]) lsl 16) land (Char.code s.[i+3]) in
          if w2 < 0xDC00 || w2 > 0xDFFF then
            raise (invalid_arg "UTF16.decode")
          else
            let u1 = w1 land 0b1111111111 in
            let u2 = w2 land 0b1111111111 in
            let u = (u1 lsl 10) lor u2 in
            let u = u + 0x10000 in
            (Uchar.of_int u, 4)
  end)

include UTF16String

include Normalization.Make(UTF16String)(UTF16String)
