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

open StringCompat

module type DataEncoding = sig
  val buf_int8 : Buffer.t -> int -> unit
  val buf_int8_64 : Buffer.t -> int64 -> unit
  val buf_int16_64 : Buffer.t -> int64 -> unit
  val buf_int32_64 : Buffer.t -> int64 -> unit
  val buf_int64 : Buffer.t -> int64 -> unit

  val str_int8_32 : bytes -> int -> int32 -> unit
  val str_int16_32 : bytes -> int -> int32 -> unit
  val str_int32 : bytes -> int -> int32 -> unit

  val get_int8_64 : string -> int -> int64 * int
  val get_int16_64 : string -> int -> int64 * int
  val get_int32_64 : string -> int -> int64 * int
  val get_int64 : string -> int -> int64 * int
  val get_uint8_64 : string -> int -> int64 * int
  val get_uint16_64 : string -> int -> int64 * int
  val get_uint32_64 : string -> int -> int64 * int
  val get_uint64 : string -> int -> int64 * int

  val get_int : string -> int -> int * int
  val get_uint : string -> int -> int * int
  val get_int8 : string -> int -> int * int
  val get_uint8 : string -> int -> int * int
  val get_int16 : string -> int -> int * int
  val get_uint16 : string -> int -> int * int

  val get_int8_32 : string -> int -> int32 * int
  val get_int16_32 : string -> int -> int32 * int
  val get_int32 : string -> int -> int32 * int

  module Byt : sig
    val get_int8_64 : bytes -> int -> int64 * int
    val get_int16_64 : bytes -> int -> int64 * int
    val get_int32_64 : bytes -> int -> int64 * int
    val get_int64 : bytes -> int -> int64 * int
    val get_uint8_64 : bytes -> int -> int64 * int
    val get_uint16_64 : bytes -> int -> int64 * int
    val get_uint32_64 : bytes -> int -> int64 * int
    val get_uint64 : bytes -> int -> int64 * int

    val get_int : bytes -> int -> int * int
    val get_uint : bytes -> int -> int * int
    val get_int8 : bytes -> int -> int * int
    val get_uint8 : bytes -> int -> int * int
    val get_int16 : bytes -> int -> int * int
    val get_uint16 : bytes -> int -> int * int

    val get_int8_32 : bytes -> int -> int32 * int
    val get_int16_32 : bytes -> int -> int32 * int
    val get_int32 : bytes -> int -> int32 * int
  end

end
