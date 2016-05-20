open DwarfUtils

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

let z32 = Ofs32 (Int32.zero)
let z64 = Ofs64 (Int64.zero)

let read_a_loc s =
    let ofs = !(s.offset) in
    let start_addr, end_addr = get_addr_offsets s in
    let blk_length = read_uint16 s in
    let blk =
        if (start_addr = z32 && end_addr = z32)
        || (start_addr = z64 && end_addr = z64) then []
        else read_expr_block s blk_length in
    { start_offset = start_addr; end_offset = end_addr; dwarf_location_description = blk; entry_offset = Int64.of_int ofs }

let rec read_locs_of_CU s =
    let curr_loc = read_a_loc s in
    if (curr_loc.start_offset = z32 && curr_loc.end_offset = z32)
    || (curr_loc.start_offset = z64 && curr_loc.end_offset = z64) then [curr_loc]
    else curr_loc :: read_locs_of_CU s
