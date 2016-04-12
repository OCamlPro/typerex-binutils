open Zipper
open DwarfUtils
open DwarfTypes
open DwarfFormat

let get_initial_length stream =
  let sixty_four_bit_indicator = 0xffffffffl in

  let first_word = read_int32 stream in
      if first_word <> sixty_four_bit_indicator then
          (DWF_32BITS, Int64.of_int32 first_word)
      else
        (DWF_64BITS, read_int64 stream)

let get_abbrev_offset s dwf =
  match dwf with
    | DWF_32BITS -> Int64.of_int32 @@ read_int32 s
    | DWF_64BITS -> read_int64 s

type dwarf_DIE_header =
    {
    format: dwarf_format;
    unit_length : int64;
    abbrev_offset : int64;
    address_size : int;
    version : int;
    initial_length_size : int;
    abbrev_offset_size : int;
    }

type dwarf_DIE =
    {
      die_ofs : int;
      die_cu_header : dwarf_DIE_header option;
      die_parent : dwarf_DIE option;
      has_children: bool;
      depth:    int;
      abbrev_nu    : int64;
      die_tag          : dwarf_TAG;
      die_attributes   : (dwarf_AT * Form.dwarf_FORM) list;
      die_attribute_vals : (int * (Class.form_class * Form_data.form_data)) list;
    }

let readAllDIE abbrev_tbl s =

    let empty_DIE =
    { die_ofs = 0;
      die_cu_header = None;
      die_parent = None;
      has_children = false;
      depth = 0;
      abbrev_nu = Int64.zero;
      die_tag = DW_TAG_user Int64.zero;
      die_attributes = [];
      die_attribute_vals = [];
    } in

    let get_abbrev_tbl ofs =
        try
            Some(Hashtbl.find abbrev_tbl ofs)
        with Not_found -> None in

    let get_abbrev_decl tbl code =
        try
            Some(Hashtbl.find tbl code)
        with Not_found -> None in

    let read_DIE_header s =
        let (dwarf_format, initial_length) = get_initial_length s in
        let version = read_int16 s in
        let abbrev_offset = get_abbrev_offset s dwarf_format in
        let address_size = read_int8 s in

        let initial_length_size = match dwarf_format with
            DWF_32BITS -> 4
            | DWF_64BITS -> 12 in

        let abbrev_offset_size = match dwarf_format with
            DWF_32BITS -> 4
            | DWF_64BITS -> 8 in

        {
        format = dwarf_format;
        unit_length = initial_length;
        abbrev_offset = abbrev_offset;
        address_size = address_size;
        version = version;
        initial_length_size = initial_length_size;
        abbrev_offset_size = abbrev_offset_size;
        } in

    let res = ref [] in

    let read_DIE_attrs d s =
         List.map (fun (n,f) -> Form.get_form s f) d.abbrev_attributes in

    let deserialize input =
        let rec helper z input =
            let (cnode, p) = z in
            match input with
            | Some (x) :: rest ->
                    let nn = Branch(x, []) in
                    let upd_z = insert_down z nn in
                    if x.has_children
                    then
                        let res = match cnode with
                            Branch(_, []) -> move_down upd_z
                            (* guaranteed to have at least 1 child *)
                            |Branch(_,_::_) -> last_child_of_pos (move_down upd_z)
                        in helper res rest
                    else helper upd_z rest
            | [None] -> z
            | [] -> z
            | None :: rest -> helper (move_up z) rest in

        match input with
            | [] -> failwith "no input"
            | None::tl -> failwith "invalid"
            | Some(hd)::tl ->
                   let (final_tree, path) = helper (leaf hd, Top) tl in
                   match path with Top -> final_tree | _ -> failwith "problem with tree building" in

    let readADIE abtbl s h cuofs =

        let lvl = ref 0 in
        let res = ref [] in
        let exit = ref true in

        let is_cu = ref true in
        let abbrev_code_ofs = ref 0 in

        while !exit do

          abbrev_code_ofs := !(s.offset);
          let die_abbrev_code = read_uleb128 s in

          match get_abbrev_decl abtbl die_abbrev_code with
                | None -> begin
                          res := !res @ [None];
                          if !lvl > 1 then
                            lvl := !lvl - 1
                          else
                              exit := false
                          end
                | Some(d) ->
                        let vals = read_DIE_attrs d s in
                        let cu =
                            match !is_cu with
                            false ->
                            {empty_DIE with
                                    die_ofs = !abbrev_code_ofs;
                                    depth = !lvl;
                                    die_attribute_vals = vals;
                                    die_tag = d.abbrev_tag;
                                    abbrev_nu = die_abbrev_code;
                                    die_attributes = d.abbrev_attributes}
                            | true ->
                            is_cu := false;
                            {empty_DIE with
                                    die_ofs = cuofs;
                                    depth = !lvl;
                                    die_cu_header = h;
                                    die_attribute_vals = vals;
                                    die_tag = d.abbrev_tag;
                                    abbrev_nu = die_abbrev_code;
                                    die_attributes = d.abbrev_attributes} in
                        res := !res @ [Some({cu with has_children = d.abbrev_has_children})];

                        match d.abbrev_has_children, !lvl with
                        | false, 0 ->
                                exit := false
                        | true, l -> begin
                                lvl := !lvl + 1;
                        end
                        | false, l -> ()
        done; !res in

    while DwarfUtils.peek s != None do

    let cu_offset = ref !(s.offset) in
    let dw_DIE_CU_header = read_DIE_header s in
    let abbrev_offset = dw_DIE_CU_header.abbrev_offset in

      begin
      match get_abbrev_tbl (Int64.to_int abbrev_offset) with
        | Some(curr_offset_tbl) ->
                begin
                    let cu_dies = readADIE curr_offset_tbl s (Some(dw_DIE_CU_header)) !cu_offset in
                    let cuu = deserialize cu_dies in
                    res := !res @ [cuu]
                end
        | None -> ()
      end;
      cu_offset := !(s.offset)

    done; !res
