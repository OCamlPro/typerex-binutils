(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2015, Jane Street Holding                                *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)

open OptionMonad

let read_int8 s =
    try Some (int_of_char (Stream.next s))
    with Stream.Failure -> Printf.printf "empty stream\n"; None

let read_int8_as_int32 s =
    match read_int8 s with
    | None -> None
    | Some x -> Some (Int32.of_int x)

let read_int8_as_int64 s =
    match read_int8 s with
    | None -> None
    | Some x -> Some (Int64.of_int x)

let read_int16 t =
  read_int8 t
  >>= fun first_byte ->
  read_int8 t
  >>= fun second_byte ->
  if Arch.big_endian then
    Some ((first_byte lsl 8) lor second_byte)
  else
    Some ((second_byte lsl 8) lor first_byte)

let read_int32 t =
  read_int8_as_int32 t
  >>= fun first_byte ->
  read_int8_as_int32 t
  >>= fun second_byte ->
  read_int8_as_int32 t
  >>= fun third_byte ->
  read_int8_as_int32 t
  >>= fun fourth_byte ->
  if Arch.big_endian then
    Some ((Int32.logor (Int32.shift_left first_byte 24)
      (Int32.logor (Int32.shift_left second_byte 16)
        (Int32.logor (Int32.shift_left third_byte 8)
          fourth_byte))))
  else
    Some ((Int32.logor (Int32.shift_left fourth_byte 24)
      (Int32.logor (Int32.shift_left third_byte 16)
        (Int32.logor (Int32.shift_left second_byte 8)
          first_byte))))

let read_int64 t =
  read_int8_as_int64 t
  >>= fun first_byte ->
  read_int8_as_int64 t
  >>= fun second_byte ->
  read_int8_as_int64 t
  >>= fun third_byte ->
  read_int8_as_int64 t
  >>= fun fourth_byte ->
  read_int8_as_int64 t
  >>= fun fifth_byte ->
  read_int8_as_int64 t
  >>= fun sixth_byte ->
  read_int8_as_int64 t
  >>= fun seventh_byte ->
  read_int8_as_int64 t
  >>= fun eighth_byte ->
  if Arch.big_endian then
    Some ((Int64.logor (Int64.shift_left first_byte 56)
      (Int64.logor (Int64.shift_left second_byte 48)
        (Int64.logor (Int64.shift_left third_byte 40)
          (Int64.logor (Int64.shift_left fourth_byte 32)
            (Int64.logor (Int64.shift_left fifth_byte 24)
              (Int64.logor (Int64.shift_left sixth_byte 16)
                (Int64.logor (Int64.shift_left seventh_byte 8)
                  eighth_byte))))))))
  else
    Some ((Int64.logor (Int64.shift_left eighth_byte 56)
      (Int64.logor (Int64.shift_left seventh_byte 48)
        (Int64.logor (Int64.shift_left sixth_byte 40)
          (Int64.logor (Int64.shift_left fifth_byte 32)
            (Int64.logor (Int64.shift_left fourth_byte 24)
              (Int64.logor (Int64.shift_left third_byte 16)
                (Int64.logor (Int64.shift_left second_byte 8)
                  first_byte))))))))

let read_null_terminated_string t =
  let buf = Buffer.create 42 in
  let result = ref None in
  while !result = None do
    match read_int8 t with
    | None as error -> result := Some error
    | Some 0 -> result := Some ( Some (Buffer.contents buf) )
    | Some c -> Buffer.add_char buf (Char.chr c)
  done;
  match !result with
    | Some (r) -> r
    | None -> Printf.printf "error\n"; None
