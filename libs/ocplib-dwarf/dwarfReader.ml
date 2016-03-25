(**************************************************************************)
(*                                                                        *)
(*  Copyright 2012 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

open Stream_in
open OptionMonad
open Leb128
open DwarfTypes
open DwarfPrinter

type at = (int, dwarf_abbreviation) Hashtbl.t

let get_initial_length stream =
  let sixty_four_bit_indicator = 0xffffffffl in

  Stream_in.read_int32 stream
  >>= fun first_word ->
      if first_word <> sixty_four_bit_indicator then
          Some(DWF_32BITS, Int64.of_int32 first_word)
      else
        Stream_in.read_int64 stream
        >>= fun initial_length ->
                Some(DWF_64BITS, initial_length)

let get_version s = Stream_in.read_int16 s

let get_address_size s = Stream_in.read_int8 s

let get_abbrev_offset s dwf =
  match dwf with
    | DWF_32BITS -> Stream_in.read_int32 s >>= (fun i32 -> Some (Int64.of_int32 i32))
    | DWF_64BITS -> Stream_in.read_int64 s

let read_abbrev_declaration s decl_code =
    let tag =
        begin
            match read_uleb128 s with
                | Some(t) -> dw_tag t
                | _ -> raise (Failure "cannot read tag")
        end in
    let has_children =
        begin
            match Stream_in.read_int8 s with
                | Some(0x01) -> true
                | Some(0x00) -> false
                | _ -> false
        end in
    let rec attr_helper s =
        match (read_uleb128 s, read_uleb128 s) with
          | Some(0), Some(0) -> []
          | Some(attr_name), Some(attr_form) -> (dw_at attr_name, dw_form attr_form) :: attr_helper s
          | _, _ -> [] in

    { abbrev_num = decl_code;
      abbrev_tag = tag;
      abbrev_has_children = has_children;
      abbrev_attributes = attr_helper s; }

let read_abbrev_section s t =
  let offset_tbl : at ref = ref (Hashtbl.create 10) in
  let exit = ref true in
  while !exit do
    let decl_code = read_uleb128 s in

    begin
    match decl_code with
    | Some(0) -> Hashtbl.add t !(s.offset) !offset_tbl;
                 offset_tbl := Hashtbl.create 10
    | Some(c) -> begin
             let abbrev_declaration = read_abbrev_declaration s (Int64.of_int c) in
             Hashtbl.add !offset_tbl c abbrev_declaration;
           end
    | None -> exit := false
    end;

    if Stream_in.peek s == None then exit := false;
  done;
  t

let read_CUs s =
  let offset = ref 0 in
  while Stream_in.peek s != None do
      get_initial_length s
      >>= fun (dwarf_format, initial_length) ->
      get_version s
      >>= fun version ->
      get_abbrev_offset s dwarf_format
      >>= fun abbrev_offset ->
      get_address_size s
      >>= fun address_size ->

      begin
      let initial_length_size = match dwarf_format with
        DWF_32BITS -> 4
        | DWF_64BITS -> 12
      in
      let abbrev_offset_size = match dwarf_format with
        DWF_32BITS ->Printf.printf "dwarf format 32 bits\n"; 4
        | DWF_64BITS -> Printf.printf "dwarf format 64 bits\n";8
      in
      let to_skip = Int64.to_int (Int64.sub initial_length (Int64.of_int (2 + 1 + abbrev_offset_size))) in
      offset := !offset + initial_length_size + abbrev_offset_size + 2 + 1;
      (*Printf.printf "now at offset %d\n" !offset;*)
      Printf.printf "initial length : %Lu\n" initial_length;
      (*Printf.printf "dwarf version : %d\n" version;*)
      Printf.printf "debug_abbrev_offset : %Lu\n" abbrev_offset;
      (*Printf.printf "address width on target arch: %d bytes\n" address_size;*)
      (*Printf.printf "bytes to skip: %d\n" to_skip;*)
      for i = 1 to to_skip do Stream_in.junk s; offset := !offset + 1 done;
      Printf.printf "now at offset %d\n" !offset;
      None
      end
  done


let read_line_prog_header s =
  let get_header_length s = match !(Flags.format) with
    | DWF_32BITS -> Stream_in.read_int32 s >>= (fun i32 -> Some (Int64.of_int32 i32))
    | DWF_64BITS -> Stream_in.read_int64 s in

  let get_standard_opcode_lengths s op_b =
      let rec helper cnt s = match cnt with 0 -> [] | _ -> match Stream_in.read_int8 s with Some(c) -> c :: helper (cnt - 1) s | _ -> [] in
      Some(helper op_b s) in

  let get_include_directories s =
      let dirs = ref [] in
      let exit = ref true in
      while !exit do
      match Stream_in.peek s with
      | Some(0) -> let _ = Stream_in.read_int8 s in exit := false
      | Some(c) -> dirs := (Stream_in.read_null_terminated_string s) :: !dirs
      | None -> exit := false
      done; Some(!dirs) in

  let get_file_names s =
      let entries = ref [] in
      let exit = ref true in
      while !exit do
      match Stream_in.peek s with
      | Some(0) -> let _ = Stream_in.read_int8 s in exit := false
      | Some(c) -> begin
                    let path = Stream_in.read_null_terminated_string s in
                    let _ = read_uleb128 s
                            >>= fun index ->
                            read_uleb128 s
                            >>= fun time ->
                            read_uleb128 s
                            >>= fun len ->
                            entries := (path, index, time, len) :: !entries; None in ()
                   end
      | None -> exit := false
      done; Some(!entries) in

  get_initial_length s
  >>= fun (dwarf_format, unit_length) ->
  get_version s
  >>= fun version ->
  get_header_length s
  >>= fun header_len ->
  Stream_in.read_int8 s
  >>= fun min_inst_len ->
  (*Section 6.2.4 5. p113 DWARF 4*)
  (*For non-VLIW architectures, this field is 1, the op_index register is always 0, and the*)
  (*operation pointer is simply the address register.*)
  (*Stream_in.read_int8 s*)
  (*>>= fun max_ops_per_inst ->*)
  Stream_in.read_int8 s
  >>= fun default_is_stmt ->
  Stream_in.read_int8 s
  >>= fun line_base ->
  Stream_in.read_int8 s
  >>= fun line_range ->
  Stream_in.read_int8 s
  >>= fun opcode_base ->
  get_standard_opcode_lengths s (opcode_base-1)
  >>= fun standard_opcode_lengths ->
  get_include_directories s
  >>= fun include_directories ->
  get_file_names s
  >>= fun file_names ->
  Some { unit_length = unit_length;
    version = version;
    header_len = header_len;
    min_inst_len = min_inst_len;
    max_ops_per_inst = 1;
    default_is_stmt = default_is_stmt;
    line_base = line_base;
    line_range = line_range;
    opcode_base = opcode_base;
    standard_opcode_lengths = standard_opcode_lengths;
    include_directories = include_directories;
    file_names = file_names; }

let read_line_prog_stmts s h =
    let read_uleb128 s = match Leb128.read_uleb128 s with
       | Some(i) -> Int64.of_int i
       | None -> Printf.kprintf failwith "pblm" in

    let read_sleb128 s = match Leb128.read_sleb128 s with
       | Some(i) -> i
       | None -> Printf.kprintf failwith "pblm" in

    let read_int8 s = match Stream_in.read_int8 s with
       | Some(i) -> i
       | None -> Printf.kprintf failwith "pblm" in

    let read_int16 s = match Stream_in.read_int16 s with
       | Some(i) -> Int64.of_int i
       | None -> Printf.kprintf failwith "pblm" in

    let read_int32 s = match Stream_in.read_int32 s with
       | Some(i) -> Int64.of_int32 i
       | None -> Printf.kprintf failwith "pblm" in

    let read_int64 s = match Stream_in.read_int64 s with
       | Some(i) -> i
       | None -> Printf.kprintf failwith "pblm" in

    let read_extended_opcode op s ofs_end =
        let dw_lne_lo_user = 0x80 in
        let dw_lne_hi_user = 0xff in
        let end_ins = !(s.offset) + ofs_end in
        match op with
          | 0x01 -> DW_LNE_end_sequence
          | 0x02 -> DW_LNE_set_address (if !(Flags.address_size_on_target) == 4 then read_int32 s else read_int64 s)
          | 0x03 -> DW_LNE_define_file ("", read_uleb128 s, read_uleb128 s, read_uleb128 s)
          | 0x04 -> DW_LNE_set_discriminator (read_uleb128 s)
          | n -> if (n >= dw_lne_lo_user) && (n <= dw_lne_hi_user)
                 then DW_LNE_user (Int64.of_int n)
                 else Printf.kprintf failwith "unknown DW_LNE opcode %x" n in

    let read_standard_opcode opc s =
        match opc with
          | 0x01 -> DW_LNS_copy
          | 0x02 -> DW_LNS_advance_pc (read_uleb128 s)
          | 0x03 -> DW_LNS_advance_line (read_sleb128 s)
          | 0x04 -> DW_LNS_set_file (read_uleb128 s)
          | 0x05 -> DW_LNS_set_column (read_uleb128 s)
          | 0x06 -> DW_LNS_negate_stmt
          | 0x07 -> DW_LNS_set_basic_block
          | 0x08 ->
                  let adjusted_opcode = 255 - h.opcode_base in
                  let address_addend = ((adjusted_opcode / h.line_range) * h.min_inst_len) in
                  DW_LNS_const_add_pc address_addend
          | 0x09 -> DW_LNS_fixed_advance_pc (read_int16 s)
          | 0x0a -> DW_LNS_set_prologue_end
          | 0x0b -> DW_LNS_set_epilogue_begin
          | 0x0c -> DW_LNS_set_isa (read_uleb128 s)
          | n -> Printf.kprintf failwith "unknown DW_LNS opcode %x" n in

    let read_special_opcode s = DW_LN_spe_op in

    let exit = ref true in
    let res = ref [] in

    while !exit do
    let curr_offset = !(s.offset) in
    match Stream_in.read_int8 s with
    | Some(0) -> let ins_len = read_uleb128 s in
                 let ext_opc = read_int8 s in
                 let result = read_extended_opcode ext_opc s (Int64.to_int (ins_len)-1) in
                 res := !res @ [(curr_offset, result)];
                 begin match ext_opc with 0x01 -> exit := false; print_endline "met end of seq" | _ -> () end
    | Some(c) when c > 0 && c < h.opcode_base -> res := !res @ [(curr_offset, read_standard_opcode c s)]
    | Some(c) when c >= h.opcode_base -> res := !res @ [(curr_offset, read_special_opcode s)]
    | Some(c) -> exit := false;
    | None -> exit := false;
    end;
    done;
    !res

let read_lineprog_section s =
  let exit = ref true in
  while Stream_in.peek s != None do
  read_line_prog_header s >>=
  fun header ->
      string_of_lineprog_header header;
      let ln = read_line_prog_stmts s header in string_of_lineprg ln; None
  done

