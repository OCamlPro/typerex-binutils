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
    DWF_32BITS -> Stream_in.read_int32 s >>= (fun i32 -> Some (Int64.of_int32 i32))
    | DWF_64BITS -> Stream_in.read_int64 s

let read_abbrev_declaration s decl_code =
    let rec attr_helper s =
        match (read_uleb128 s, read_uleb128 s) with
          Some(0), Some(0) -> []
          | Some(attr_name), Some(attr_form) -> (dw_at attr_name, dw_form attr_form) :: attr_helper s
          | _, _ -> [] in
    let tag = (begin match read_uleb128 s with Some(t) -> dw_tag t | _ -> raise (Failure "cannot read tag") end) in
    let has_children = begin
        match Stream_in.read_int8 s with
        Some(0x01) -> true
        | Some(0x00) -> false
        | _ -> false
        end in
    { abbrev_num = decl_code;
      abbrev_tag = tag;
      abbrev_has_children = has_children;
      abbrev_attributes = attr_helper s;
    }

let rec read_abbrev_section s abbrev_tbl =
  (*let offset = ref 0 in*)
  (*let abbrev_tbl : at = Hashtbl.create 10 in*)
  (*while Stream.peek s != None do*)
    let decl_code = read_uleb128 s in

    begin
    match decl_code with
    | Some(0) -> read_abbrev_section s abbrev_tbl (*put old map in abbrev map then create new map for next offset*)
    | Some(c) -> begin
             (*let new_abbrev_table_for_offset = Hashtbl.create 50 in*)
             let abbrev_declaration = read_abbrev_declaration s (Int64.of_int c) in
             (*read_abbrev_declaration s (Int64.of_int c);*)
             Hashtbl.add abbrev_tbl c abbrev_declaration;
             (*abbrev_tbl*)
             (*read_abbrev_section s abbrev_tbl*)
           end
    (*| Some (c) -> Printf.printf "%d\n" c;*)
    | None -> abbrev_tbl
    end
  (*done*)

let read_CUs s =
  let offset = ref 0 in
  while Stream.peek s != None do
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
      for i = 1 to to_skip do Stream.junk s; offset := !offset + 1 done;
      Printf.printf "now at offset %d\n" !offset;
      None
      end
  done;

