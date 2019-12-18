open Types

module type EncDec =
  sig
    (*
     * Encode a unicode character into a string.
     * Argument:
     *   u : the unicode character.
     * Returns a string containing exactly the number of bytes of the encoded
     * character. Raise invalid_arg if u is not in the unicode range.
     *)
    val encode : Uchar.t -> string

    (*
     * Decode a unicode character at a given position in a string.
     * Arguments:
     *   s : the string,
     *   i : index where to look.
     * Returns a couple (u, l) where u is the code of the character and l is
     * the number of bytes read.
     * Raise invalid_arg if s.[i] is not a valid first byte for an encoded
     * character, if the encoding is not valid or if s is not long enough.
     *)
    val decode : string -> int -> (Uchar.t * int)
  end

module type UTFString = sig
  val encode : Uchar.t -> string
  val decode : string -> int -> (Uchar.t * int)
  val validate : string -> bool
  val fold : ('a -> Uchar.t -> 'a) -> 'a -> string -> 'a
  val map : (Uchar.t -> Uchar.t) -> string -> string
  val nth_index : string -> int -> int
  val length : string -> int
  val sub : string -> int -> int -> string
  val nth : string -> int -> (Uchar.t * int)
  val next : string -> int -> int
  val prev : string -> int -> int
  val trim : string -> string
  val init : int -> (int -> Uchar.t) -> string
  val empty_string : string
  val of_list : Uchar.t list -> string
  val to_list : string -> Uchar.t list
  val grapheme_break : string -> int -> bool
  val next_grapheme : string -> int -> int
  val prev_grapheme : string -> int -> int
  val fold_left_grapheme : ('a -> string -> 'a) -> 'a -> string -> 'a
  val fold_right_grapheme : (string -> 'a -> 'a) -> string -> 'a -> 'a
  val map_grapheme : (string -> string) -> string -> string
end

module Make = functor ( ED : EncDec ) ->
  struct
    include ED

    (*
     * Decode a unicode character at a given position in a string.
     * Arguments:
     *   s : the string,
     *   i : index where to look.
     * Returns the code of the character.
     * Raise invalid_arg if s.[i] is not a valid first byte for an encoded
     * character, if the encoding is not valid or if s is not long enough.
     *)
    let look : string -> int -> Uchar.t = fun s i ->
      fst (decode s i)

    (*
     * Check a string for correct encoding.
     * Argument:
     *   s : the string.
     * Returns true in case of success, and false in case of error.
     * All exceptions are captured.
     *)
    let validate : string -> bool = fun s ->
      let len = String.length s in
      let rec valid i =
        if i = len then
          true
        else if i > len then
          false
        else
          let (_, sz) = decode s i in
          valid (i + sz)
      in
      try valid 0 with _ -> false

    (*
     * Fold function on each character of an encoded string.
     * Argument:
     *   f   : the fold function,
     *   ini : the initial value of the accumulator,
     *   s   : the string.
     * Returns the content of the accumulator once the full string has been
     * scanned.
     *)
    let fold : ('a -> Uchar.t -> 'a) -> 'a -> string -> 'a = fun f ini s ->
      if s = "" then
        ini
      else
        let l = String.length s in
        let rec fold' acc i =
          if i > l then
            raise (invalid_arg "UTF.fold")
          else if i = l then
            acc
          else
            let (u, l) = decode s i in
            fold' (f acc u) (i + l)
        in fold' ini 0

    let map : (Uchar.t -> Uchar.t) -> string -> string = fun fn s ->
      let l = fold (fun acc c -> encode (fn c) :: acc) [] s in
      String.concat "" (List.rev l)

    (*
     * Compute the length of an encoded unicode string.
     * Argument:
     *   s : the string (that is supposed to be valid).
     * Returns an int.
     *)
    let length : string -> int = fun s ->
      fold (fun i _ -> i + 1) 0 s

    (*
     * Compute the index of the next unicode character encoded in a string
     * starting from a given index.
     * Arguments:
     *   s : the string (that is supposed to be valid),
     *   i : the index of the current encoded character.
     * Returns the index of the next encoded character.
     * The behaviour is not specified if i is not a valid index in s, or if s
     * is not a valid unicode string.
     *)
    let next : string -> int -> int = fun s i ->
      let (_, sz) = decode s i in
      i + sz

    let sub s start len =
      let slen = String.length s in
      let rec find pos num =
        if num < 0 then invalid_arg "Utf8.sub (negative index)";
        if num = 0 then pos else
          begin
            if pos >= slen then invalid_arg "Utf8.sub (char out of bound)";
            find (next s pos) (num-1)
          end
      in
      let start = find 0 start in
      let len   = (find start len) - start in
      String.sub s start len

    (*
     * Test whether an index is out of range (i.e. if it points out of the
     * string borders).
     *   s : the string (that is supposed to be valid),
     *   i : the index of the current encoded character.
     * Returns true if the index is out of range, false otherwise.
     *)
    let out_of_range : string -> int -> bool = fun s i ->
      i < 0 || i >= String.length s

    (*
     * Compute the index of the previous unicode character encoded in a string
     * starting from a given index.
     * Arguments:
     *   s : the string (that is supposed to be valid),
     *   i : the index of the current encoded character.
     * Returns the index of the next encoded character.
     * The behaviour is not specified if i is not a valid index in s, or if s is
     * not a valid unicode string.
     *)
    let prev : string -> int -> int = fun s i ->
      let rec try_until_found n =
        if n > 4 then invalid_arg "Utf8.prev" else
          (try
             let p = i - n in
             let (_, sz) = decode s p in
             if (p + sz) <> i then try_until_found (n+1) else p
           with Invalid_argument _ -> try_until_found (n+1)) in
      try_until_found 1

    (*
     * Compute the index of the n-th unicode character ecoded in a string if
     * it exists.
     * Arguments:
     *   s : the string (that is supposed to be valid),
     *   n : the character number (should be greater or equal to 0).
     * Returns the index.
     * Raise Out_of_bound in case the string is not long enough.
     * if negatif: from end, -1, then last one
     *)
    let nth_index : string -> int -> int = fun s n ->
      let len = String.length s in
      let rec nth_ind count pos =
        if pos >= len then
          raise Out_of_bound
        else if count = 0 then
          pos
        else
          let pos = next s pos in
          nth_ind (count - 1) pos
      in
      let rec rev_nth_ind count pos =
        if pos < 0 then
          raise Out_of_bound
        else if count = 0 then
          pos
        else
          let pos = prev s pos in
          rev_nth_ind (count + 1) pos
      in if n >= 0 then nth_ind n 0 else rev_nth_ind n (String.length s)


    (*
     * Compute the value of the n-th unicode character encoded in a string if
     * it exists.
     * Arguments:
     *   s : the string (that is supposed to be valid),
     *   n : the character number (should be greater or equal to 0).
     * Returns the character.
     * Raise Out_of_bound in case the string is not long enough.
     *)
    let nth : string -> int -> (Uchar.t * int) = fun s n ->
      let pos = nth_index s n in
      decode s pos

    (*
     * Compute the index of the first unicode character in a string.
     * Argument:
     *   s : the string (that is supposed to be valid).
     * Returns the first unicode character index in s.
     *)
    let first : string -> int = fun _ ->
      0

    (*
     * Compute the index of the last unicode character in a string.
     * Argument:
     *   s : a non-empty string (that is supposed to be valid).
     * Returns the index of the last unicode  character in s.
     * Raise invalid_arg if the string s is empty.
     *)
    let last : string -> int = fun s ->
      let len = String.length s in
      if len = 0 then
        raise (invalid_arg "UTF.last")
      else
        prev s len

    (*
     * Returns a copy of the given string, without leading and trailing
     * whitespace characters (see UChar.is_space).
     *)
    let trim : string -> string = fun s ->
      let l = ref 0 in
      let r = ref (last s) in
      while CharInfo.is_space (look s !l) do l := next s !l done;
      while CharInfo.is_space (look s !r) do r := prev s !r done;
      String.sub s !l ((next s !r) - !l)

    (*
     * Buffer for unicode strings.
     *)
    module Buffer =
      struct
        include Buffer
        type buf = string

        let add_char buf u =
          let s = encode u in
          for i = 0 to String.length s - 1 do
            Buffer.add_char buf s.[i]
          done

        let init len f =
          let buf = Buffer.create len in
          for c = 0 to len - 1 do
            add_char buf (f c)
          done;
          buf
      end

    (*
     * Create an encoded string using a function.
     * Arguments:
     *   len : the length of the string to create (in number of [uchar]),
     *   f   : the initialization function.
     * Returns the created string, which i-th character will be encoded using
     * [f i].
     *)
    let init : int -> (int -> Uchar.t) -> string = fun len f ->
      let b = Buffer.create (2 * len) in
      for i = 1 to len do
        Buffer.add_char b (f i)
      done;
      Buffer.contents b

    let of_list : Uchar.t list -> string = fun l ->
      let b = Buffer.create 1024 in
      List.iter (fun c -> Buffer.add_char b c) l;
      Buffer.contents b

    let to_list : string -> Uchar.t list = fun s ->
      List.rev (fold (fun acc x -> x::acc) [] s)

    (*
     * Empty encoded string.
     *)
    let empty_string : string = ""

    let grapheme_break : string -> int -> bool = fun s pos ->
      let open CharInfo in
      let get_bp c =
        try
          (get_char_descr c).grapheme_break
        with
          Not_found -> Other
      in
      if pos = 0 || pos >= String.length s then true else
        let i0 = prev s pos in
        let c1 = look s i0 and c2 = look s pos in
        let bp1 = get_bp c1 and bp2 = get_bp c2 in
        let rec previous_ri acc i0 bp1 =
          match bp1 with
          | RegionalIndicator ->
             begin
               if i0 = 0 then not acc else
                 let i0 = prev s i0 in
                 let c1 = look s i0 in
                 let bp1 = get_bp c1 in
                 previous_ri (not acc) i0 bp1
             end
          | _ -> acc
        in
        let rec previous_pict i0 bp1 =
          match bp1 with
          | Extend ->
             begin
               if i0 = 0 then false else
                 let i0 = prev s i0 in
                 let c1 = look s i0 in
                 let bp1 = get_bp c1 in
                 previous_pict i0 bp1
             end
          | ExtPict -> true
          | _       -> false
        in
        let previous =
          match bp1 with
          | ZWJ ->
             if previous_pict i0 Extend then ExtPictExtendStar
             else NoPrevious
          | RegionalIndicator ->
             if previous_ri false i0 RegionalIndicator then EvenRegionalIndicator
             else NoPrevious
          | _ -> NoPrevious
        in
        break_between previous bp1 bp2

    let next_grapheme : string -> int -> int = fun s pos ->
      let npos = ref pos in
      try
        while !npos < String.length s do
          npos := next s !npos;
          if grapheme_break s !npos then raise Exit
        done;
        raise Not_found (* end of string *)
      with
        Exit -> !npos

    let prev_grapheme : string -> int -> int = fun s pos ->
      let npos = ref pos in
      try
        while !npos > 0 do
          try
            npos := prev s !npos;
            if grapheme_break s !npos then raise Exit
          with
            Invalid_argument _ -> decr npos
        done;
        0
      with
        Exit -> !npos

    let fold_left_grapheme : ('a -> string -> 'a) -> 'a -> string -> 'a =
      fun fn acc s ->
        let pos = ref 0 in
        let res = ref acc in
        while !pos < String.length s do
          let npos = next_grapheme s !pos in
          let s = String.sub s !pos (npos - !pos) in
          pos := npos;
          res := fn !res s;
        done;
        !res

    let fold_right_grapheme : (string -> 'a -> 'a) -> string -> 'a -> 'a =
      fun fn s acc ->
        let pos = ref (String.length s)  in
        let res = ref acc in
        while !pos > 0 do
          let npos = prev_grapheme s !pos in
          let s = String.sub s npos (!pos - npos) in
          pos := npos;
          res := fn s !res;
        done;
        !res

    let map_grapheme : (string -> string) -> string -> string = fun fn s ->
      let l = fold_left_grapheme (fun acc c -> fn c :: acc) [] s in
      String.concat "" (List.rev l)


  end
