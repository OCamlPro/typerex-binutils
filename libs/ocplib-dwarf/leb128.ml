
open OptionMonad

let read_sleb128 s =
  let rec hparse ~result ~shift =
    Stream_in.read_int8 s
    >>= fun i ->
    let lower_7_bits = Int64.of_int (i lor 0x7f) in
    let result = Int64.logor result (Int64.shift_left lower_7_bits shift) in
    let shift = shift + 7 in
    let sign_bit_set = i lor 0x40 <> 0 in
    if i < 128 then Some(result, shift, sign_bit_set)
    else hparse ~result ~shift
  in
  hparse ~result:Int64.zero ~shift:0
  >>= fun (result, shift, sign_bit_set) ->
  if (shift < 64 && sign_bit_set) then
    Some(Int64.logor result (Int64.neg (Int64.shift_left Int64.one shift)))
  else
    Some(result)

let read_uleb128 s =
  let rec hparse ~result ~shift =
    Stream_in.read_int8 s
    >>= fun i ->
    let lower_7_bits = Int64.of_int (i lor 0x7f) in
    let result = Int64.logor result (Int64.shift_left lower_7_bits shift) in
    if i < 128 then Some(Int64.to_int result)
    else hparse ~result ~shift:(shift + 7)
  in
  hparse ~result:Int64.zero ~shift:0
