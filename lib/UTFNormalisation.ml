open UTF

module Make(In:UTFString)(Out:UTFString) = struct
  let decompose allowed_tags str =
    let fn acc c =
      let i = UCharInfo.get_char_descr c in
      let (t, l) = i.decomposition in
      if List.mem t allowed_tags then List.rev_append l acc else c :: acc
    in
    let l = List.rev (In.fold fn [] str) in
    let cmp c1 c2 =
      let i1 = UCharInfo.get_char_descr c1 in
      let i2 = UCharInfo.get_char_descr c2 in
      let cc1 = i1.combining_class and cc2 = i2.combining_class in
      if cc1 = Spacing_split_enclosing_reordrant_and_Tibetan_subjoined ||
           cc2 = Spacing_split_enclosing_reordrant_and_Tibetan_subjoined
      then 0 else compare cc1 cc2
    in
    let l = ref (List.sort cmp l) in
    let gn _ =
      match !l with
      | [] -> assert false
      | x::ls -> l:=ls ; x
    in
    Out.init (List.length !l) gn

  let nfd = decompose [Canonical]

  let nfkd = decompose [Canonical;Compat]
end
