open Zipper
open DwarfUtils
open Form_data
open DwarfTypes
open DwarfDIE

type address_offset = Ofs32 of int32 | Ofs64 of int64

(*A location list entry consists of two address offsets followed by a 2-byte length, followed by a*)
(*block of contiguous bytes that contains a DWARF location description. The length specifies the*)
(*number of bytes in that block.*)
(*The two offsets are the same size as an address on the target machine.*)

type location_list_entry =
  { start_offset : address_offset;
    end_offset : address_offset;
    dwarf_location_description : int list;
    entry_offset : int64;
  }

(*Each entry in a location list is either a location list entry, a base address selection entry, or an end*)
(*of list entry.*)
(*For a location list to be specified, the base address of the corresponding compilation unit must be*)
(*defined (see Section 3.1.1).*)

(*Relevant lines in section 3.1.1*)
(*Compilation unit entries may have the following attributes: [..]*)
(*A DW_AT_low_pc attribute may also be specified in combination with DW_AT_ranges to*)
(*specify the default base address for use in location lists (see Section 2.6.2) and range lists*)
(*(see Section 2.17.3).*)

let get_addr_offsets s =
  match Arch.address_size with
    | 4 -> let s1 = read_int32 s in let e1 = read_int32 s in Ofs32 s1, Ofs32 e1
    | _ -> let s1 = read_int64 s in let e1 = read_int64 s in Ofs64 s1, Ofs64 e1

let rec read_expr_block s l =
  if l <=0 then [] else let byte = read_uint8 s in byte :: read_expr_block s (l-1)

let z32 = Ofs32 (Int64.to_int32 Int64.zero)
let z64 = Ofs64 (Int64.zero)

let read_a_loc s =
  let ofs = !(s.offset) in
  let start_addr, end_addr = get_addr_offsets s in
  let blk =
    if (start_addr = z32 && end_addr = z32)
    || (start_addr = z64 && end_addr = z64) then []
    else
      let blk_length = read_uint16 s in
      read_expr_block s blk_length in
  { start_offset = start_addr; end_offset = end_addr; dwarf_location_description = blk; entry_offset = Int64.of_int ofs }

let rec read_locs s =
  let curr_loc = read_a_loc s in
  if (curr_loc.start_offset = z32 && curr_loc.end_offset = z32)
  || (curr_loc.start_offset = z64 && curr_loc.end_offset = z64) then [curr_loc]
  else curr_loc :: read_locs s

(*Use of difference list more elegant as it avoids recursive cons and list reversal*)
let rec read_all_locs s =
  let rec h s acc =
      if DwarfUtils.peek s == None then acc []
    else let loc = read_locs s in h s (fun ys -> acc (loc :: ys)) in
  h s (fun ys -> ys)

let rec attr2val target_at ats vals = match ats, vals with
  |(at, _) :: tl1 , (_, (_, v)) :: tl2 -> if target_at == at then [v] else attr2val target_at tl1 tl2
  | ((_, _)::_, [])
  | ([], _::_)
  | [], [] -> []

let attr_val x v = attr2val x v.die_attributes v.die_attribute_vals
let get_ofs_from_attr a v =
  match attr_val a v with
  | [x] -> begin match x with | (OFS_I32 (i)) -> Int64.of_int32 i
                        | (OFS_I64 (i)) -> i
                        | _ -> failwith "not a offset" end
  | [] | _::_::_ -> failwith "attr not found"

let get_name v str_sec =
  let string_of_ofs ofs = read_null_terminated_string
                      {str_sec with offset = ref (Int64.to_int ofs)} in
  string_of_ofs (get_ofs_from_attr DW_AT_name v)

let get_loc v = get_ofs_from_attr DW_AT_location v

let read_caml_locs loc_section cu debug_str_sec =

  let curr_spn = ref "" in

  let rec find_subp z lres =
    try
      let ptr = go_ahead z in
      let ctree = current_tree ptr in
      let cval = current_value' ptr in
      let pv =
        begin match cval.die_tag with
          | DW_TAG_subprogram -> begin
            if List.length cval.die_attributes == 3 then curr_spn := get_name cval debug_str_sec;

            if List.length cval.die_attributes == 5
            then begin

              let sp_low_pc = get_ofs_from_attr DW_AT_low_pc cval in
              fold_tree (fun x l ->
                  let res =
                      match x.die_tag with
                          | DW_TAG_variable -> [(!curr_spn, sp_low_pc, true, x)]
                          | DW_TAG_formal_parameter -> [(!curr_spn, sp_low_pc, false, x)]
                          | _ -> [] in
                  res @ List.concat l) ctree
            end
            else []
          end
          | _ -> []
        end in
      find_subp ptr (lres @ pv)
    with _ -> lres in

  let pv = find_subp (cu, Top) [] in
  let pv_map = Hashtbl.create 10 in

  List.iter (fun (spn, sppc, var, atrs) ->
    Hashtbl.add pv_map (get_loc atrs) (spn, (get_name atrs debug_str_sec), sppc, var)) pv;

  let locs = List.map
    (fun (_,_,_,atrs) ->
        read_locs {loc_section with offset =  ref (Int64.to_int @@ get_loc atrs)}
    ) pv
  in locs, pv_map

let get_c_params_and_vars cu debug_str_sec =

  let curr_spn = ref "" in

  let rec find_subp z lres =
    try
      let ptr = go_ahead z in
      let ctree = current_tree ptr in
      let cval = current_value' ptr in
      let pv =
        begin match cval.die_tag with
          | DW_TAG_subprogram -> begin
              curr_spn := get_name cval debug_str_sec;

              let sp_low_pc = get_ofs_from_attr DW_AT_low_pc cval in

              fold_tree (fun x l ->
                  let res =
                      match x.die_tag with
                          | DW_TAG_variable -> [(!curr_spn, sp_low_pc, true, x)]
                          | DW_TAG_formal_parameter -> [(!curr_spn, sp_low_pc, false, x)]
                          | _ -> [] in
                  res @ List.concat l) ctree
          end
          | _ -> []
        end in
      find_subp ptr (lres @ pv)
    with _ -> lres in

  find_subp (cu, Top) []
