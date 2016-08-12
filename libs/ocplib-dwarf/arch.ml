(*We assume the target binary is x86_64*)

type t = LittleEndian | BigEndian

let endianness = LittleEndian

let is_big_endian =
  match endianness with
  | LittleEndian -> false
  | _ -> true

let address_size = 8
