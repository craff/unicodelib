open UTF

module Make(In:UTFString)(Out:UTFString) = struct
  let decompose allowed_tags str =
    let rec insert c1 l =
      match l with
      | [] -> [c1]
      | c2::l' as l ->
         let i1 = UCharInfo.get_char_descr c1 in
         let i2 = UCharInfo.get_char_descr c2 in
         let cc1 = UTFTypes.combining_class_to_int (i1.combining_class) in
         let cc2 = UTFTypes.combining_class_to_int (i2.combining_class) in
         if cc1 = 0 || cc2 = 0 then c1::l
         else if cc1 < cc2 then c2 :: insert c1 l' else c1 :: l
    in
    let rec fn acc c =
      let i = UCharInfo.get_char_descr c in
      match i.decomposition with
      | None -> insert c acc
      | Some(t,l) ->
         if List.mem t allowed_tags then List.fold_left fn acc l else insert c acc
    in
    Out.of_list (List.rev (In.fold fn [] str))

  let nfd = decompose [Canonical]

  let nfkd = decompose [Canonical;Compat;NoBreak;Super;Fraction;Sub;
                        Font;Circle;Wide;Vertical;Square;Initial;
                        Narrow;Final;Isolated;Medial;Small]
end
