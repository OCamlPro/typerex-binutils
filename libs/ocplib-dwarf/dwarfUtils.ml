open Arch

(*what happens when I reach the end of the string?*)

module LE = EndianString.LittleEndian
module BE = EndianString.BigEndian

type s = { str: string; offset: int ref }

let wrap lf bf c =
    fun s ->
        let res = match Arch.endianness with
        | LittleEndian -> lf s.str !(s.offset)
        | BigEndian -> bf s.str !(s.offset) in
        s.offset := !(s.offset) + c; res

let read_char = wrap LE.get_char BE.get_char 1

let read_int8 = wrap LE.get_int8 BE.get_int8 1
let read_uint8 = wrap LE.get_uint8 BE.get_uint8 1

let read_int16 = wrap LE.get_int16 BE.get_int16 2
let read_uint16 = wrap LE.get_uint16 BE.get_uint16 2

let read_int32 = wrap LE.get_int32 BE.get_int32 4

let read_int64 = wrap LE.get_int64 BE.get_int64 8

let uint8_to_int8 x =
    let shift = Sys.word_size - 8 - 1 in
    (x lsl shift) asr shift
