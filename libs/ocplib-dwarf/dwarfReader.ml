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

open DwarfTypes
open DwarfPrinter

let get_initial_length stream =
  let sixty_four_bit_indicator = 0xffffffffl in

  Stream_in.read_int32 stream
  |> fun first_word ->
      if first_word <> sixty_four_bit_indicator then
          (DWF_32BITS, Int64.of_int32 first_word)
      else
        Stream_in.read_int64 stream
        |> fun initial_length ->
                (DWF_64BITS, initial_length)

let get_version s = Stream_in.read_int16 s

let get_address_size s = Stream_in.read_int8 s

let get_abbrev_offset s dwf =
  match dwf with
    DWF_32BITS -> Int64.of_int32 @@ Stream_in.read_int32 s
    | DWF_64BITS -> Stream_in.read_int64 s

let read_section_header s =
  let (dwarf_format, initial_length) = get_initial_length s in
  let version = get_version s in
  let abbrev_offset = get_abbrev_offset s dwarf_format in
  let address_size = get_address_size s in

  begin
  match dwarf_format with
    DWF_32BITS -> Printf.printf "dwarf format 32 bits\n";
    | DWF_64BITS -> Printf.printf "dwarf format 64 bits\n";
  end;
  Printf.printf "initial length : %Lu\n" initial_length;
  Printf.printf "dwarf version : %d\n" version;
  Printf.printf "debug_abbrev_offset : %Lu\n" abbrev_offset;
  Printf.printf "address width on target arch: %d bytes\n" address_size

let read_sleb128 s =
  let rec hparse ~result ~shift =
    Stream_in.read_int8 s
    |> fun i ->
    let lower_7_bits = Int64.of_int (i lor 0x7f) in
    let result = Int64.logor result (Int64.shift_left lower_7_bits shift) in
    let shift = shift + 7 in
    let sign_bit_set = i lor 0x40 <> 0 in
    if i < 128 then (result, shift, sign_bit_set)
    else hparse ~result ~shift
  in
  hparse ~result:Int64.zero ~shift:0
  |> fun (result, shift, sign_bit_set) ->
  if (shift < 64 && sign_bit_set) then
    (Int64.logor result (Int64.neg (Int64.shift_left Int64.one shift)))
  else
    result

let read_uleb128 s =
  let rec hparse ~result ~shift =
    Stream_in.read_int8 s
    |> fun i ->
    let lower_7_bits = Int64.of_int (i lor 0x7f) in
    let result = Int64.logor result (Int64.shift_left lower_7_bits shift) in
    if i < 128 then Int64.to_int result
    else hparse ~result ~shift:(shift + 7)
  in
  hparse ~result:Int64.zero ~shift:0

type at = (Word64.t, dwarf_abbreviation) Hashtbl.t

(*type dwarf_abbreviation =*)
    (*{ abbrev_num : Word64.t;*)
      (*abbrev_tag : dwarf_TAG;*)
      (*abbrev_has_children : bool;*)
      (*abbrev_attributes : (dwarf_AT * dwarf_FORM) list;*)
    (*}*)

let read_abbrev_declaration s decl_code =
    let rec attr_helper s =
        match (read_uleb128 s, read_uleb128 s) with
          0, 0 -> []
          | attr_name, attr_form -> (dw_at attr_name, dw_form attr_form) :: attr_helper s in
    { abbrev_num = decl_code;
      abbrev_tag = dw_tag @@ read_uleb128 s;
      (*abbrev_has_children = match Stream_in.read_int8 s with 0x01 -> true | _ -> false;*)
      abbrev_has_children = true;
      abbrev_attributes = attr_helper s;
    }

let rec read_abbrev_section s abbrev_tbl =
    let zero = Int64.zero in
    let decl_code = read_uleb128 s in
    match Int64.compare decl_code (Int64.zero) with
    | 0 -> abbrev_tbl
    | _ -> begin
                let abbrev_declaration = read_abbrev_declaration s decl_code in
                Hashtbl.add abbrev_tbl decl_code abbrev_declaration;
                read_abbrev_section s abbrev_tbl
                end

let read s = ()
