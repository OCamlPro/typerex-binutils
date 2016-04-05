open DwarfTypes
open DwarfFormat

let get_initial_length stream =
  let sixty_four_bit_indicator = 0xffffffffl in

  let first_word = DwarfUtils.read_int32 stream in
      if first_word <> sixty_four_bit_indicator then
          (DWF_32BITS, Int64.of_int32 first_word)
      else
          let initial_length = DwarfUtils.read_int64 stream in
                (DWF_64BITS, initial_length)


let get_version s = DwarfUtils.read_int16 s

let get_address_size s = DwarfUtils.read_int8 s

let get_abbrev_offset s dwf =
  match dwf with
    | DWF_32BITS -> Int64.of_int32 @@ DwarfUtils.read_int32 s
    | DWF_64BITS -> DwarfUtils.read_int64 s

type dwarf_DIE =
    { die_id : Word64.t; (* Unique identifier for this entry. *)
      (* Unique identifier of this entry's parent. *)
      die_parent : Word64.t option;
      (* Unique identifiers of this entry's children. *)
      die_children : Word64.t list;
      (* Unique identifier of the left sibling in the DIE tree,
         if one exists. *)
      die_sibling_left  : Word64.t option;
      (* Unique identifier of the right sibling in the DIE tree,
         if one exists. *)
      die_sibling_right : Word64.t option;
      (* Type tag. *)
      die_tag          : dwarf_TAG;
      (* Attribute tag and value pairs. *)
      die_attributes   : (dwarf_AT * dwarf_ATVAL) list;

      die_attribute_vals : (Class.form_class, Form_data.form_data) Form.t list;
    }

let readAllDIE abbrev_tbl s =

    let offset = ref 0 in

    let empty_DIE =
    { die_id = Int64.zero;
      die_parent = None;
      die_children = [];
      die_sibling_left = None;
      die_sibling_right = None;
      die_tag = DW_TAG_user Int64.zero;
      die_attributes = [];
      die_attribute_vals = [];
    } in

    let get_abbrev_tbl ofs =
        try
            Some(Hashtbl.find abbrev_tbl ofs)
        with Not_found -> None in

    let read_CU abbrev_ofs s = () in

  (*= List.map (fun (n,f) -> printf "%s %s\n" (string_of_AT n) (string_of_FORM f)) d.abbrev_attributes in ()*)

    let res = ref [] in

    while DwarfUtils.peek s != None do

    let (dwarf_format, initial_length) = get_initial_length s in
    let version = get_version s in
    let abbrev_offset = get_abbrev_offset s dwarf_format in
    let address_size = get_address_size s in

    let initial_length_size = match dwarf_format with
        DWF_32BITS -> 4
        | DWF_64BITS -> 12 in

    let abbrev_offset_size = match dwarf_format with
        DWF_32BITS ->Printf.printf "dwarf format 32 bits\n"; 4
        | DWF_64BITS -> Printf.printf "dwarf format 64 bits\n";8 in

      begin
      let to_skip = Int64.to_int (Int64.sub initial_length (Int64.of_int (2 + 1 + abbrev_offset_size))) in
      offset := !offset + initial_length_size + abbrev_offset_size + 2 + 1;
      Printf.printf "now at offset %d\n" !offset;
      Printf.printf "initial length : %Lu\n" initial_length;
      Printf.printf "dwarf version : %d\n" version;
      Printf.printf "debug_abbrev_offset : %Lu\n" abbrev_offset;
      let curr_offset_tbl = get_abbrev_tbl (Int64.to_int abbrev_offset) in
      if curr_offset_tbl == None then
          Printf.printf "abbrev for offset %Lu not found\n" abbrev_offset
      else
          begin
              Printf.printf "abbrev for offset %Lu found\n" abbrev_offset;
              read_CU curr_offset_tbl s ;
              ()
          end;
      Printf.printf "address width on target arch: %d bytes\n" address_size;
      Printf.printf "bytes to skip: %d\n" to_skip;
      for i = 1 to to_skip do DwarfUtils.junk s; offset := !offset + 1 done;
      Printf.printf "now at offset %d\n" !offset;
      end

    done
