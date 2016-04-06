open Form_data
open Class
open DwarfFormat
open DwarfUtils

type dwarf_FORM =
    DW_FORM_addr              (*   address *)
  | DW_FORM_block2 (*   block *)
  | DW_FORM_block4 (*   block *)
  | DW_FORM_data2 (*   constant *)
  | DW_FORM_data4 (*   constant, lineptr, loclistptr, macptr, rangelistptr *)
  | DW_FORM_data8 (*   constant, lineptr, loclistptr, macptr, rangelistptr *)
  | DW_FORM_string (*   string *)
  | DW_FORM_block (*   block *)
  | DW_FORM_block1 (*   block *)
  | DW_FORM_data1 (*   constant *)
  | DW_FORM_flag (*   flag *)
  | DW_FORM_sdata (*   constant *)
  | DW_FORM_strp (*   string *)
  | DW_FORM_udata (*   constant *)
  | DW_FORM_ref_addr            (*   reference *)
  | DW_FORM_ref1                (*   reference *)
  | DW_FORM_ref2                (*   reference *)
  | DW_FORM_ref4                (*   reference *)
  | DW_FORM_ref8                (*   reference *)
  | DW_FORM_ref_udata           (*   reference *)
  | DW_FORM_indirect            (*   (see Section 7.5.3 of DWARF3 specification) *)
  | DW_FORM_sec_offset      (* lineptr, loclistptr, macptr, rangelistptr *)
  | DW_FORM_exprloc         (* exprloc *)
  | DW_FORM_flag_present    (* flag *)
  | DW_FORM_ref_sig8        (* reference *)

let dw_form =
  function
    0x01 -> DW_FORM_addr
  | 0x03 -> DW_FORM_block2
  | 0x04 -> DW_FORM_block4
  | 0x05 -> DW_FORM_data2
  | 0x06 -> DW_FORM_data4
  | 0x07 -> DW_FORM_data8
  | 0x08 -> DW_FORM_string
  | 0x09 -> DW_FORM_block
  | 0x0a -> DW_FORM_block1
  | 0x0b -> DW_FORM_data1
  | 0x0c -> DW_FORM_flag
  | 0x0d -> DW_FORM_sdata
  | 0x0e -> DW_FORM_strp
  | 0x0f -> DW_FORM_udata
  | 0x10 -> DW_FORM_ref_addr
  | 0x11 -> DW_FORM_ref1
  | 0x12 -> DW_FORM_ref2
  | 0x13 -> DW_FORM_ref4
  | 0x14 -> DW_FORM_ref8
  | 0x15 -> DW_FORM_ref_udata
  | 0x16 -> DW_FORM_indirect
  | 0x17 -> DW_FORM_sec_offset
  | 0x18 -> DW_FORM_exprloc
  | 0x19 -> DW_FORM_flag_present
  | 0x20 -> DW_FORM_ref_sig8
  | n -> Printf.kprintf failwith "unknown DW_FORM %x" n

let string_of_FORM =
  function
    DW_FORM_addr ->"DW_FORM_addr"
  | DW_FORM_block2 ->"DW_FORM_block2"
  | DW_FORM_block4 ->"DW_FORM_block4"
  | DW_FORM_data2 ->"DW_FORM_data2"
  | DW_FORM_data4 ->"DW_FORM_data4"
  | DW_FORM_data8 ->"DW_FORM_data8"
  | DW_FORM_string ->"DW_FORM_string"
  | DW_FORM_block ->"DW_FORM_block"
  | DW_FORM_block1 ->"DW_FORM_block1"
  | DW_FORM_data1 ->"DW_FORM_data1"
  | DW_FORM_flag ->"DW_FORM_flag"
  | DW_FORM_sdata ->"DW_FORM_sdata"
  | DW_FORM_strp ->"DW_FORM_strp"
  | DW_FORM_udata ->"DW_FORM_udata"
  | DW_FORM_ref_addr ->"DW_FORM_ref_addr"
  | DW_FORM_ref1 ->"DW_FORM_ref1"
  | DW_FORM_ref2 ->"DW_FORM_ref2"
  | DW_FORM_ref4 ->"DW_FORM_ref4"
  | DW_FORM_ref8 ->"DW_FORM_ref8"
  | DW_FORM_ref_udata ->"DW_FORM_ref_udata"
  | DW_FORM_indirect ->"DW_FORM_indirect"
  | DW_FORM_sec_offset -> "DW_FORM_sec_offset"
  | DW_FORM_exprloc -> "DW_FORM_exprloc"
  | DW_FORM_flag_present -> "DW_FORM_flag_present"
  | DW_FORM_ref_sig8 -> "DW_FORM_ref_sig8"

let get_form s f =
    let read_flag s = match read_int8 s with 0x00 -> false | _ -> true in
    let read_block l s =
        let length = ref l in
        let arr = ref [] in
        while !length != 0 do
            arr := !arr @ [read_char s];
            length := !length - 1
        done;
        !arr in

    match f with
        DW_FORM_addr -> if !Flags.address_size_on_target == 4
                        then (`address, OFS_I32 (read_int32 s))
                        else (`address, OFS_I64 (read_int64 s))
      (*blocks are arrays*)
      (*1 byte length followed by up to 255 bytes*)
      | DW_FORM_block1 -> let length = read_int8 s in
                            (`block, Block1 (length, read_block length s))
      (*2 - 65k*)
      | DW_FORM_block2 -> let length = read_int16 s in
                          (`block, Block2 (length, read_block length s))
      (*4 - 4B*)
      | DW_FORM_block4 -> let length = read_int32 s in
                          (`block, Block4 (length, read_block (Int32.to_int length) s))
      (*a uleb128 length - number of bytes specified*)
      | DW_FORM_block -> let length = read_uleb128 s in
                         (`block, Block (length, read_block (Int64.to_int length) s))
      | DW_FORM_data1 -> (`constant, Data1 (read_char s))
      | DW_FORM_data2 -> (`constant, Data2 (read_int16 s))
      | DW_FORM_data4 ->  (`constant, Data4 (read_int32 s))
      | DW_FORM_data8 ->  (`constant, Data8 (read_int64 s))
      | DW_FORM_sdata -> (`constant, Sdata (read_sleb128 s))
      | DW_FORM_udata -> (`constant, Udata (read_uleb128 s))

      | DW_FORM_string -> (`string, String (read_null_terminated_string s))
      | DW_FORM_strp -> if !DwarfFormat.format == DWF_32BITS
                        then (`string, OFS_I32 (read_int32 s))
                        else (`string, OFS_I64 (read_int64 s))

      | DW_FORM_flag -> (`flag, Flag (read_flag s))
      | DW_FORM_flag_present -> (`flag, FlagPresent)

      | DW_FORM_ref1 -> (`reference, Ref1 (read_char s))
      | DW_FORM_ref2 -> (`reference, Ref2 (read_int16 s))
      | DW_FORM_ref4 -> (`reference, Ref4 (read_int32 s))
      | DW_FORM_ref8 -> (`reference, Ref8 (read_int64 s))
      | DW_FORM_ref_udata -> (`reference, Ref_udata (read_uleb128 s))
      | DW_FORM_ref_sig8 ->
            if !DwarfFormat.format == DWF_32BITS
            then
              (`reference, OFS_I32 (read_int32 s))
            else
              (`reference, OFS_I64 (read_int64 s))
      | DW_FORM_ref_addr ->
            if !DwarfFormat.format == DWF_32BITS
            then
              (`reference, OFS_I32 (read_int32 s))
            else
              (`reference, OFS_I64 (read_int64 s))
    | DW_FORM_indirect -> (`indirect, Udata (read_uleb128 s))
    | DW_FORM_sec_offset ->
            if !DwarfFormat.format == DWF_32BITS
            then
              (`ptr, OFS_I32 (read_int32 s))
            else
              (`ptr, OFS_I64 (read_int64 s))

    (*is a block*)
    | DW_FORM_exprloc ->
                        let length = read_uleb128 s in
                        (`exprloc, Exprloc (length, read_block (Int64.to_int length) s))

