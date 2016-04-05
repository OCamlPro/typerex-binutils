open Arch

(*what happens when I reach the end of the string?*)

module LE = EndianString.LittleEndian
module BE = EndianString.BigEndian

type s = { str: string; offset: int ref }

let of_string s = { str = s; offset = ref 0; }

let wrap_peek lf bf = fun s -> try
    let res = match Arch.endianness with
        | LittleEndian -> lf s.str !(s.offset)
        | BigEndian -> bf s.str !(s.offset) in Some(res)
    with _ -> None

let peek = wrap_peek LE.get_int8 BE.get_int8

let wrap lf bf c =
    fun s ->
        try
        let res = match Arch.endianness with
        | LittleEndian -> lf s.str !(s.offset)
        | BigEndian -> bf s.str !(s.offset) in
        s.offset := !(s.offset) + c; res
        with _ -> Printf.kprintf failwith "end of string reached\n"

let read_char = wrap LE.get_char BE.get_char 1

let read_int8 = wrap LE.get_int8 BE.get_int8 1
let read_uint8 = wrap LE.get_uint8 BE.get_uint8 1

let read_int16 = wrap LE.get_int16 BE.get_int16 2
let read_uint16 = wrap LE.get_uint16 BE.get_uint16 2

let read_int32 = wrap LE.get_int32 BE.get_int32 4

let read_int64 = wrap LE.get_int64 BE.get_int64 8

let junk s = read_int8 s

let uint8_to_int8 x =
    let shift = Sys.word_size - 8 - 1 in
    (x lsl shift) asr shift

let read_sleb128 s =
  let rec hparse ~result ~shift =
    let i = read_int8 s in
    let lower_7_bits = Int64.of_int (i land 0x7f) in
    let result = Int64.logor result (Int64.shift_left lower_7_bits shift) in
    let shift = shift + 7 in
    let sign_bit_set = i land 0x40 <> 0 in
    if i < 128 then (result, shift, sign_bit_set)
    else hparse ~result ~shift
  in

  let (result, shift, sign_bit_set) = hparse ~result:Int64.zero ~shift:0 in
  if (shift < 64 && sign_bit_set) then
    Int64.logor result (Int64.neg (Int64.shift_left Int64.one shift))
  else
    result

let read_uleb128 s =
  let rec hparse ~result ~shift =
    let i = read_int8 s in
    let lower_7_bits = Int64.of_int (i land 0x7f) in
    let result = Int64.logor result (Int64.shift_left lower_7_bits shift) in
    if i < 128 then result
    else hparse ~result ~shift:(shift + 7)
  in
  hparse ~result:Int64.zero ~shift:0

let read_null_terminated_string t =
  let buf = Buffer.create 42 in
  let result = ref None in
  while !result = None do
    match read_int8 t with
    | 0 -> result := Some (Buffer.contents buf)
    | c -> Buffer.add_char buf (Char.chr c)
  done;
  match !result with
    | Some (r) -> r
    | None -> Printf.printf "error\n"; ""
