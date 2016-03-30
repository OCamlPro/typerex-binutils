open Form_data
open Class
open DwarfTypes
open DwarfUtils

type ('dwarf_classes, 'form) t =
  | DWF_Addr : form_class * form_data -> (form_class, form_data) t
  | DWF_Block : form_class * form_data -> (form_class, form_data) t
  | DWF_Block1 : form_class * form_data -> (form_class, form_data) t
  | DWF_Block2 : form_class * form_data -> (form_class, form_data) t
  | DWF_Block4 : form_class * form_data -> (form_class, form_data) t
  | DWF_Data1 : form_class * form_data -> (form_class, form_data) t
  | DWF_Data2 : form_class * form_data -> (form_class, form_data) t
  | DWF_Data4 : form_class * form_data -> (form_class, form_data) t
  | DWF_Data8 : form_class * form_data -> (form_class, form_data) t
  | DWF_Sdata : form_class * form_data -> (form_class, form_data) t
  | DWF_Udata : form_class * form_data -> (form_class, form_data) t
  | DWF_String : form_class * form_data -> (form_class, form_data) t
  | DWF_Flag : form_class * form_data -> (form_class, form_data) t
  | DWF_Strp : form_class * form_data -> (form_class, form_data) t
  | DWF_Ref_addr : form_class * form_data -> (form_class, form_data) t
  | DWF_Ref1 : form_class * form_data -> (form_class, form_data) t
  | DWF_Ref2 : form_class * form_data -> (form_class, form_data) t
  | DWF_Ref4 : form_class * form_data -> (form_class, form_data) t
  | DWF_Ref8 : form_class * form_data -> (form_class, form_data) t
  | DWF_Ref_udata : form_class * form_data -> (form_class, form_data) t
  | DWF_Ref_sig8 : form_class * form_data -> (form_class, form_data) t
  | DWF_Indirect : form_class * form_data -> (form_class, form_data) t
  | DWF_Sec_offset : form_class * form_data -> (form_class, form_data) t
  | DWF_Exprloc : form_class * form_data -> (form_class, form_data) t
  | DWF_Flag_present : form_class * form_data -> (form_class, form_data) t
  | DWF_None

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
                        then DWF_Addr (`address, OFS_I32 (read_int32 s))
                        else DWF_Addr (`address, OFS_I64 (read_int64 s))

      (*blocks are arrays*)
      (*1 byte length followed by up to 255 bytes*)
      | DW_FORM_block1 -> let length = read_int8 s in
                            DWF_Block1 (`block, Block1 (length, read_block length s))
      (*2 - 65k*)
      | DW_FORM_block2 -> let length = read_int16 s in
                          DWF_Block2 (`block, Block2 (length, read_block length s))
      (*4 - 4B*)
      (*| DW_FORM_block4 ->*)
                          (*let length = read_int32 s in*)
                          (*DWF_Block4 (`block, Block4 (length, read_block (Int32.to_int length) s))*)
      (*a uleb128 length - number of bytes specified*)
      | DW_FORM_block -> let length = read_uleb128 s in
                            DWF_Block (`block, Block (length, read_block (Int64.to_int length) s))
      | DW_FORM_data1 -> DWF_Data1 (`constant, Data1 (read_char s))
      | DW_FORM_data2 -> DWF_Data2 (`constant, Data2 (read_int16 s))
      | DW_FORM_data4 -> DWF_Data4 (`constant, Data4 (read_int32 s))
      | DW_FORM_data8 -> DWF_Data8 (`constant, Data8 (read_int64 s))
      | DW_FORM_sdata -> DWF_Sdata (`constant, Sdata (read_sleb128 s))
      | DW_FORM_udata -> DWF_Udata (`constant, Udata (read_uleb128 s))

      | DW_FORM_string -> DWF_String (`string, String (read_null_terminated_string s))

      | DW_FORM_strp -> if !Flags.format == DWF_32BITS
                        then DWF_Strp (`string, OFS_I32 (read_int32 s))
                        else DWF_Strp (`string, OFS_I64 (read_int64 s))

      | DW_FORM_flag -> DWF_Flag (`flag, Flag (read_flag s))
      | DW_FORM_flag_present -> DWF_Flag (`flag, FlagPresent)

      | DW_FORM_ref1 -> DWF_Ref1 (`reference, Ref1 (read_char s))
      | DW_FORM_ref2 -> DWF_Ref2 (`reference, Ref2 (read_int16 s))
      | DW_FORM_ref4 -> DWF_Ref4 (`reference, Ref4 (read_int32 s))
      | DW_FORM_ref8 -> DWF_Ref8 (`reference, Ref8 (read_int64 s))
      | DW_FORM_ref_udata -> DWF_Ref_udata (`reference, Ref_udata (read_uleb128 s))
      | DW_FORM_ref_sig8 ->
            if !Flags.format == DWF_32BITS
            then
              DWF_Ref_sig8 (`reference, OFS_I32 (read_int32 s))
            else
              DWF_Ref_sig8 (`reference, OFS_I64 (read_int64 s))
      | DW_FORM_ref_addr ->
            if !Flags.format == DWF_32BITS
            then
              DWF_Ref_addr (`reference, OFS_I32 (read_int32 s))
            else
              DWF_Ref_addr (`reference, OFS_I64 (read_int64 s))
    | DW_FORM_indirect -> DWF_Indirect(`indirect, Udata (read_uleb128 s))
    | DW_FORM_sec_offset ->
            if !Flags.format == DWF_32BITS
            then
              DWF_Sec_offset (`ptr, OFS_I32 (read_int32 s))
            else
              DWF_Sec_offset (`ptr, OFS_I64 (read_int64 s))

    (*is a block*)
    | DW_FORM_exprloc ->
                        let length = read_uleb128 s in
                        DWF_Exprloc (`exprloc, Exprloc (length, read_block (Int64.to_int length) s))
    | _ -> DWF_None

