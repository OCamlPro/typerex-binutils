(**************************************************************************)
(*                                                                        *)
(*                        OCamlPro Typerex                                *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the LGPL v3.0            *)
(*   (GNU Lesser General Public Licence version 3.0).                     *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)


open StringCompat

  (* to test DataEncoding... *)
let test msg signed buf get =
(*
  for i = 1 to 1_000_000 do
  let i = Random.int 0x3ffffff in
  let i = if signed && Random.int 10 < 5 then -i else i in
  let i = Int64.of_int i in
  let b = Buffer.create 10 in
  buf b i;
  let s = Buffer.contents b in
  let ii, _ = get s 0 in
  if ( i <> ii) then begin
  Printf.fprintf stderr "%s: %Lx <> %Lx\n" msg i ii;
  for i = 0 to String.length s - 1 do
  Printf.fprintf stderr "%02x" (int_of_char s.[i])
  done;
  Printf.fprintf stderr "\n%!";
  exit 1
  end
  done
*)
  ()

let buf_int8 b i =
  Buffer.add_char b (char_of_int (i land 0xff))

let buf_int8_64 b iL =
  buf_int8 b (Int64.to_int iL)

let buf_int16_64 b iL =
  buf_int8_64 b (Int64.shift_right iL 8);
  buf_int8_64 b iL;
  ()

let buf_int32_64 b iL =
  buf_int16_64 b (Int64.shift_right iL 16);
  buf_int16_64 b iL;
  ()

let buf_int64 b iL =
  buf_int32_64 b (Int64.shift_right iL 32);
  buf_int32_64 b iL;
  ()

let str_int8_32 s pos v =
  Bytes.set s pos (char_of_int ((Int32.to_int v) land 0xff))

let str_int16_32 s pos v =
  str_int8_32 s (pos+1) v;
  str_int8_32 s pos (Int32.shift_right_logical v 8)

let str_int32 s pos v =
  str_int16_32 s (pos+2) v;
  str_int16_32 s pos (Int32.shift_right_logical v 16);
  ()

module Make(String: sig
  type t
  val get : t -> int -> char
end) = struct

  let get_uint8_64 s pos = Int64.of_int (int_of_char s.[pos])

  let get_uint16_64 s pos =
    let c2 = get_uint8_64 s pos in
    let c1 = get_uint8_64 s (pos+1) in
    Int64.logor c1 (Int64.shift_left c2 8), pos+2

  let get_uint32_64 s pos =
    let c2,pos = get_uint16_64 s pos in
    let c1,pos = get_uint16_64 s pos in
    Int64.logor c1 (Int64.shift_left c2 16), pos

  let get_uint64 s pos =
    let c2,pos = get_uint32_64 s pos in
    let c1,pos = get_uint32_64 s pos in
    Int64.logor c1 (Int64.shift_left c2 32), pos

  let get_int8_64 s pos =
    let i = int_of_char s.[pos] in
    let i = if i > 127 then i - 256 else i in
    Int64.of_int i

  let get_int16_64 s pos =
    let c2 = get_int8_64 s pos in
    let c1 = get_uint8_64 s (pos+1) in
    Int64.logor c1 (Int64.shift_left c2 8), pos+2

  let get_int32_64 s pos =
    let c2,pos = get_int16_64 s pos in
    let c1,pos = get_uint16_64 s pos in
    Int64.logor c1 (Int64.shift_left c2 16), pos

  let get_int64 s pos =
    let c2,pos = get_int32_64 s pos in
    let c1,pos = get_uint32_64 s pos in
    Int64.logor c1 (Int64.shift_left c2 32), pos

  let get_uint8_64 s pos = get_uint8_64 s pos, pos+1
  let get_int8_64 s pos = get_int8_64 s pos, pos+1

  let get_int s pos =
    let intL, pos = get_int32_64 s pos in
    Int64.to_int intL, pos

  let get_uint s pos =
    let intL, pos = get_uint32_64 s pos in
    Int64.to_int intL, pos

  let get_int8 s pos =
    let intL, pos = get_int8_64 s pos in
    Int64.to_int intL, pos

  let get_uint8 s pos =
    let intL, pos = get_uint8_64 s pos in
    Int64.to_int intL, pos

  let get_int16 s pos =
    let intL, pos = get_int16_64 s pos in
    Int64.to_int intL, pos

  let get_uint16 s pos =
    let intL, pos = get_uint16_64 s pos in
    Int64.to_int intL, pos

  let get_int8_32 s pos =
    Int32.of_int (Char.code s.[pos]), pos+1
  let get_int16_32 s pos =
    let c1, pos = get_int8_32 s pos in
    let c2, pos = get_int8_32 s pos in
    Int32.logor c2 (Int32.shift_left c1 8), pos

  let get_int32 s pos =
    let c1, pos = get_int16_32 s pos in
    let c2, pos = get_int16_32 s pos in
    Int32.logor c2 (Int32.shift_left c1 16), pos

end


module Str = Make(String)
module Byt = Make(Bytes)
include Str


      (*
  let _ =
    test "BigEndian.get_int32_64" true buf_int32_64 get_int32_64;
    test "BigEndian.get_uint32_64" false buf_int32_64 get_uint32_64
      *)
