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

let read_section_header s =
  let (dwarf_format, initial_length) = get_initial_length s in
  Printf.printf "initial length : %Lu\n" initial_length

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

let read s = ()
