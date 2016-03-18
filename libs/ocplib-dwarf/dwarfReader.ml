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
    let read_extended_opcode s = () in
    let read_standard_opcode opc s = () in
    let read_special_opcode s = () in
    let exit = ref true in
    while !exit do
    match Stream_in.read_int8 s with
    | Some(0) -> read_extended_opcode s
    | Some(c) when c > 0 && c < h.opcode_base -> read_standard_opcode c s
    | Some(c) when c >= h.opcode_base -> read_special_opcode s
    | Some(c) -> exit := false;
    | None -> exit := false;
    done

  (*.debug_line*)
let read_lineprog_section s =
  let _ = read_line_prog_header s >>=
  fun header ->
      string_of_lineprog_header header; read_line_prog_stmts s header; None in ()
