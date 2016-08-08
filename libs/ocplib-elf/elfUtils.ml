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

open ElfTypes

let wrap lf bf =
  fun e b pos ->
    if e.byte_order = LittleEndian then lf b pos else bf b pos

(*
let get_half = wrap LittleEndian.get_uint16_64 BigEndian.get_uint16_64
let get_addr32 : encoding -> string -> int -> int64 * int= wrap LittleEndian.get_uint32_64 BigEndian.get_uint32_64
let get_addr64 = wrap LittleEndian.get_int64 BigEndian.get_int64
let get_off32 = wrap LittleEndian.get_uint32_64 BigEndian.get_uint32_64
let get_off64 = wrap LittleEndian.get_int64 BigEndian.get_int64
let get_sword = wrap LittleEndian.get_int32_64 BigEndian.get_int32_64
let get_word32 = wrap LittleEndian.get_uint32_64 BigEndian.get_uint32_64
let get_word64 = wrap LittleEndian.get_uint64 BigEndian.get_uint64
  *)


let get_uchar = wrap LittleEndian.get_uint8_64 BigEndian.get_uint8_64
let get_char = wrap LittleEndian.get_int8_64 BigEndian.get_int8_64

let get_Elf32_Addr = wrap LittleEndian.get_uint32_64 BigEndian.get_uint32_64
let get_Elf32_Half = wrap LittleEndian.get_uint16_64 BigEndian.get_uint16_64
let get_Elf32_Off = wrap LittleEndian.get_uint32_64 BigEndian.get_uint32_64
let get_Elf32_Sword = wrap LittleEndian.get_int32_64 BigEndian.get_int32_64
let get_Elf32_Word = wrap LittleEndian.get_uint32_64 BigEndian.get_uint32_64

let get_Elf64_Addr = wrap LittleEndian.get_uint64 BigEndian.get_uint64
let get_Elf64_Off = wrap LittleEndian.get_uint64 BigEndian.get_uint64
let get_Elf64_Half = wrap LittleEndian.get_uint16_64 BigEndian.get_uint16_64
let get_Elf64_Word = wrap LittleEndian.get_uint32_64 BigEndian.get_uint32_64
let get_Elf64_Sword = wrap LittleEndian.get_int32_64 BigEndian.get_int32_64
let get_Elf64_Xword = wrap LittleEndian.get_int64 BigEndian.get_int64
let get_Elf64_Sxword = wrap LittleEndian.get_uint64 BigEndian.get_uint64

let get_Elfxx_Word = get_Elf32_Word
let get_Elfxx_Half = get_Elf32_Half
let get_Elfxx_Off en s pos =
  if en.word_size = ARCH32 then get_Elf32_Off en s pos else
    get_Elf64_Off en s pos

let get_Elfxx_Addr en s pos =
  if en.word_size = ARCH32 then get_Elf32_Addr en s pos else
    get_Elf64_Addr en s pos

let get_word_by_class en s pos =
  if en.word_size = ARCH32 then
    get_Elf32_Word en s pos else
    get_Elf64_Xword en s pos

let wrap lf bf =
  fun e b v ->
    if e.byte_order = LittleEndian then lf b v else bf b v

(* TODO: use buf_uint16_64 instead of buf_int16_64 *)
let buf_half64 = wrap LittleEndian.buf_int16_64 BigEndian.buf_int16_64
let buf_half e b v = buf_half64 e b (Int64.of_int v)
let buf_Elfxx_Half = wrap LittleEndian.buf_int16_64 BigEndian.buf_int16_64
let buf_Elf32_Half = buf_Elfxx_Half
let buf_Elf64_Half = buf_Elfxx_Half

let buf_Elfxx_Word = wrap LittleEndian.buf_int32_64 BigEndian.buf_int32_64
let buf_Elf32_Word = buf_Elfxx_Word
let buf_Elf64_Word = buf_Elfxx_Word

let buf_word32 = wrap LittleEndian.buf_int32_64 BigEndian.buf_int32_64
let buf_word64 = wrap LittleEndian.buf_int64 BigEndian.buf_int64

let buf_addr32 = wrap LittleEndian.buf_int32_64 BigEndian.buf_int32_64
let buf_addr64 = wrap LittleEndian.buf_int64 BigEndian.buf_int64
let buf_addr_by_class e b v =
  if e.word_size = ARCH32 then
    buf_addr32 e b v
  else
    buf_addr64 e b v
let buf_word_by_class e b v =
  if e.word_size = ARCH32 then
    buf_word32 e b v
  else
    buf_word64 e b v
let buf_off_by_class = buf_word_by_class
let buf_off32 = buf_word32
let buf_Elf32_Off = buf_word32
let buf_Elf64_Off = buf_word64
let buf_Elfxx_Off e b v =
  if e.word_size = ARCH32 then
    buf_Elf32_Off e b v
  else
    buf_Elf64_Off e b v

let shn_undef = 0       (* undefined *)
let shn_abs = 0xFFF1    (* absolute value *)
let shn_common = 0xFFF2 (* common section *)

let shn_undefL = 0L       (* undefined *)
let shn_absL = 0xFFF1L    (* absolute value *)
let shn_commonL = 0xFFF2L (* common section *)



let get_encoding e_data_encoding e_file_class =
    let byte_order = match e_data_encoding with
      | ELFDATA2LSB -> LittleEndian
      | ELFDATA2MSB -> BigEndian
      | _ ->
        failwith "unsupported data encoding"
    in
    let word_size = match e_file_class with
      | ELFCLASS32 -> ARCH32
      | ELFCLASS64 -> ARCH64
      | _ -> failwith "unsupported file class"
    in
    { byte_order; word_size }
