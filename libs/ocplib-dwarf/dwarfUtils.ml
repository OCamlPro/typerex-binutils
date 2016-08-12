open Arch
open DwarfFormat

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
      s.offset := !(s.offset) + c;
      res
    with _ -> Printf.kprintf failwith "end of string reached"

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

let int16_to_uint16 x =
  let shift = Sys.word_size - 16 - 1 in
  (x lsl shift) lsr shift

let read_sleb128 s =
  let rec hparse ~result ~shift =
    let i = read_uint8 s in
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
    let i = read_uint8 s in
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
  | None -> Printf.kprintf failwith "error trying to read string\n"

let get_initial_length stream =
  let sixty_four_bit_indicator = 0xffffffffl in

  let first_word = read_int32 stream in
  if first_word <> sixty_four_bit_indicator then
    (DWF_32BITS, Int64.of_int32 first_word)
  else
    (DWF_64BITS, read_int64 stream)

let sleb128_from_list s =
  let rec hparse ~result ~shift l =
    match l with
    | [] -> failwith "sleb128_from_list : empty list"
    | i :: tl ->
      let lower_7_bits = Int64.of_int (i land 0x7f) in
      let result = Int64.logor result (Int64.shift_left lower_7_bits shift) in
      let shift = shift + 7 in
      let sign_bit_set = i land 0x40 <> 0 in
      if i < 128 then (tl, result, shift, sign_bit_set)
      else hparse ~result ~shift tl
  in
  let (r, result, shift, sign_bit_set) = hparse ~result:Int64.zero ~shift:0 s in
  if (shift < 64 && sign_bit_set) then
    (r, Int64.logor result (Int64.neg (Int64.shift_left Int64.one shift)))
  else
    (r, result)

let uleb128_from_list s =
  let rec hparse ~result ~shift l =
    match l with
    | [] -> failwith "uleb128_from_list : empty list"
    | i :: tl ->
      let lower_7_bits = Int64.of_int (i land 0x7f) in
      let result = Int64.logor result (Int64.shift_left lower_7_bits shift) in
      if i < 128 then (tl, result)
      else hparse ~result ~shift:(shift + 7) tl
  in
  hparse ~result:Int64.zero ~shift:0 s

let int16_from_list t =
  match t with
  | first_byte :: second_byte :: tl ->
    if Arch.is_big_endian then
      (tl, (first_byte lsl 8) lor second_byte)
    else
      (tl, (second_byte lsl 8) lor first_byte)
  | r -> (r, 0)

let int32_from_list t =
  match t with
  | b1 :: b2 :: b3 :: b4 :: tl ->
    let b1 = Int32.of_int b1 in
    let b2 = Int32.of_int b2 in
    let b3 = Int32.of_int b3 in
    let b4 = Int32.of_int b4 in
    if Arch.is_big_endian then
      (tl, (Int32.logor (Int32.shift_left b1 24)
              (Int32.logor (Int32.shift_left b2 16)
                 (Int32.logor (Int32.shift_left b3 8)
                    b4))))
    else
      (tl, (Int32.logor (Int32.shift_left b4 24)
              (Int32.logor (Int32.shift_left b3 16)
                 (Int32.logor (Int32.shift_left b2 8)
                    b1))))
  | r -> (r, Int32.zero)

let int64_from_list t =
  match t with
  | b1 :: b2 :: b3 :: b4 ::
    b5 :: b6 :: b7 :: b8 :: tl ->

    let b1 = Int64.of_int b1 in
    let b2 = Int64.of_int b2 in
    let b3 = Int64.of_int b3 in
    let b4 = Int64.of_int b4 in
    let b5 = Int64.of_int b5 in
    let b6 = Int64.of_int b6 in
    let b7 = Int64.of_int b7 in
    let b8 = Int64.of_int b8 in
    if Arch.is_big_endian then
      (tl, (Int64.logor (Int64.shift_left b1 56)
              (Int64.logor (Int64.shift_left b2 48)
                 (Int64.logor (Int64.shift_left b3 40)
                    (Int64.logor (Int64.shift_left b4 32)
                       (Int64.logor (Int64.shift_left b5 24)
                          (Int64.logor (Int64.shift_left b6 16)
                             (Int64.logor (Int64.shift_left b7 8)
                                b8))))))))
    else
      (tl, (Int64.logor (Int64.shift_left b8 56)
              (Int64.logor (Int64.shift_left b7 48)
                 (Int64.logor (Int64.shift_left b6 40)
                    (Int64.logor (Int64.shift_left b5 32)
                       (Int64.logor (Int64.shift_left b4 24)
                          (Int64.logor (Int64.shift_left b3 16)
                             (Int64.logor (Int64.shift_left b2 8)
                                b1))))))))
  | r -> (r, Int64.zero)

