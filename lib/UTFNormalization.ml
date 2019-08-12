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
         if allowed_tags t then List.fold_left fn acc l else insert c acc
    in
    Out.of_list (List.rev (In.fold fn [] str))

  let nfd = decompose (fun t -> t = Canonical)

  let nfkd = decompose (fun _ -> true)

  let recompose str =
    let open UTFTypes in

    (* starter and same combining class do not commute *)
    let starter c = (UCharInfo.get_char_descr c).combining_class = Starter in

    let same_cc c1 c2 =
      (UCharInfo.get_char_descr c1).combining_class =
        (UCharInfo.get_char_descr c2).combining_class
    in

    (* try to apply fn to all characters that can be grought to head in ls *)
    let iter_commutable fn ls =
      let rec gn acc ls =
        match ls with
        | [] -> raise Not_found
        | y::ls -> if (acc = [] || not (starter y))
                        &&  not (List.exists (same_cc y) acc)
                   then
                     try fn y (List.rev_append acc ls)
                     with Not_found when not (starter y) -> gn (y::acc) ls
                   else
                     if not (starter y) then gn (y::acc) ls
                     else raise Not_found
      in
      gn [] ls
    in
    let rec fn acc l =
      match l with
      | [] -> List.rev acc
      | x::ls ->
         let rec gn (Node tbl) x ls =
           let (r,tbl) = Hashtbl.find tbl x in
           match r, ls with
           | (None, _::_) -> iter_commutable (gn tbl) ls
           | (Some c, _)   -> (try iter_commutable (gn tbl) ls (* try long first *)
                              with Not_found -> fn acc (c::ls))
           | (_, [])       -> raise Not_found
         in
         try iter_commutable (gn (Lazy.force UCharInfo.prefix_tree)) l
         with Not_found -> fn (x::acc) ls
    in
    Out.of_list (fn [] (In.to_list str))

  let nfc str = recompose (nfd str)

  let nfkc str = recompose (nfkd str)
end
