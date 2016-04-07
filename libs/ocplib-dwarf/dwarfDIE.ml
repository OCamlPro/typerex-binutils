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
      mutable die_children : dwarf_DIE list;
      die_tag          : dwarf_TAG;
      die_attributes   : (dwarf_AT * Form.dwarf_FORM) list;
      die_attribute_vals : (int * (Class.form_class * Form_data.form_data)) list;
    }

let readAllDIE abbrev_tbl s =

    let empty_DIE =
    { die_ofs = 0;
      die_cu_header = None;
      die_parent = None;
      die_children = [];
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

    let rec readADIE abtbl s lvl h parent_die =

          (*Printf.printf "now in level %d\n" lvl;*)
          let abbrev_code_ofs = !(s.offset) in
          let die_abbrev_code = read_uleb128 s in
          let new_die = empty_DIE in

          match get_abbrev_decl abtbl die_abbrev_code with
                        (*null DIEs are caught here*)
                        | None ->
                                (*Printf.printf "abbrev decl for code %Ld not found\n" die_abbrev_code;*)
                                  if lvl > 1 then readADIE abtbl s (lvl-1) None parent_die else
                                      begin
                                          match parent_die with
                                          Some(d) -> d
                                            | None ->
                                                    (*print_endline "damn"; *)
                                                    empty_DIE
                                      end
                        | Some(d) ->
                                let vals = read_DIE_attrs d s in
                                let cu =
                                    match parent_die with
                                    Some(pd) ->
                                    {new_die with
                                            die_ofs = abbrev_code_ofs;
                                            die_parent = parent_die;
                                            die_attribute_vals = vals;
                                            die_tag = d.abbrev_tag;
                                            die_attributes = d.abbrev_attributes}
                                    | None ->
                                    {new_die with
                                            die_cu_header = h;
                                            die_attribute_vals = vals;
                                            die_tag = d.abbrev_tag;
                                            die_attributes = d.abbrev_attributes} in

                              begin
                              (*Printf.printf "abbrev decl for code %Ld found\n" die_abbrev_code;*)
                                            if d.abbrev_has_children then begin
                                                (*Printf.printf "<%d>going down lvl %d\n" lvl (lvl+1);*)
                                                let child = readADIE abtbl s (lvl+1) None (Some cu) in
                                                cu.die_children <- cu.die_children @ [child];
                                                cu
                                            end
                                            else
                                                if lvl == 0 then cu else
                                                let md = readADIE abtbl s lvl None cu.die_parent in md
                                        end in

    while DwarfUtils.peek s != None do

    let cu_offset = ref !(s.offset) in
    let dw_DIE_CU_header = read_DIE_header s in
    let abbrev_offset = dw_DIE_CU_header.abbrev_offset in

      begin
      match get_abbrev_tbl (Int64.to_int abbrev_offset) with
        | Some(curr_offset_tbl) -> begin
                                    let cuu = readADIE curr_offset_tbl s 0 (Some dw_DIE_CU_header) None in
                                    res := !res @ [{cuu with die_ofs = !cu_offset}]
                                    end
        | None -> ()
      end;
      cu_offset := !(s.offset)

    done; !res
