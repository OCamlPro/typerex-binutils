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

open DwarfTypes

(*Section 2.5 pg 31*)
(*DWARF expressions describe how to compute a value or name a location during debugging of a*)
(*program. They are expressed in terms of DWARF operations that operate on a stack of values.*)

(*A DWARF expression is stored in a block of contiguous bytes. The bytes form a sequence of*)
(*operations. Each operation is a 1-byte code that identifies that operation, followed by zero or*)
(*more bytes of additional data.*)

type dwarf_OP =
    DW_OP_addr of Word64.t
  | DW_OP_deref
  | DW_OP_const1u of Word8.t
  | DW_OP_const1s of Int8.t
  | DW_OP_const2u of Word16.t
  | DW_OP_const2s of Int16.t
  | DW_OP_const4u of Word32.t
  | DW_OP_const4s of Int32.t
  | DW_OP_const8u of Word64.t
  | DW_OP_const8s of Int64.t
  | DW_OP_constu  of Word64.t
  | DW_OP_consts  of Int64.t
  | DW_OP_dup
  | DW_OP_drop
  | DW_OP_over
  | DW_OP_pick of Word8.t
  | DW_OP_swap
  | DW_OP_rot
  | DW_OP_xderef
  | DW_OP_abs
  | DW_OP_and
  | DW_OP_div
  | DW_OP_minus
  | DW_OP_mod
  | DW_OP_mul
  | DW_OP_neg
  | DW_OP_not
  | DW_OP_or
  | DW_OP_plus
  | DW_OP_plus_uconst of Word64.t
  | DW_OP_shl
  | DW_OP_shr
  | DW_OP_shra
  | DW_OP_xor
  | DW_OP_skip of Int16.t
  | DW_OP_bra of Int16.t
  | DW_OP_eq
  | DW_OP_ge
  | DW_OP_gt
  | DW_OP_le
  | DW_OP_lt
  | DW_OP_ne
  | DW_OP_lit0
  | DW_OP_lit1
  | DW_OP_lit2
  | DW_OP_lit3
  | DW_OP_lit4
  | DW_OP_lit5
  | DW_OP_lit6
  | DW_OP_lit7
  | DW_OP_lit8
  | DW_OP_lit9
  | DW_OP_lit10
  | DW_OP_lit11
  | DW_OP_lit12
  | DW_OP_lit13
  | DW_OP_lit14
  | DW_OP_lit15
  | DW_OP_lit16
  | DW_OP_lit17
  | DW_OP_lit18
  | DW_OP_lit19
  | DW_OP_lit20
  | DW_OP_lit21
  | DW_OP_lit22
  | DW_OP_lit23
  | DW_OP_lit24
  | DW_OP_lit25
  | DW_OP_lit26
  | DW_OP_lit27
  | DW_OP_lit28
  | DW_OP_lit29
  | DW_OP_lit30
  | DW_OP_lit31
  | DW_OP_reg0
  | DW_OP_reg1
  | DW_OP_reg2
  | DW_OP_reg3
  | DW_OP_reg4
  | DW_OP_reg5
  | DW_OP_reg6
  | DW_OP_reg7
  | DW_OP_reg8
  | DW_OP_reg9
  | DW_OP_reg10
  | DW_OP_reg11
  | DW_OP_reg12
  | DW_OP_reg13
  | DW_OP_reg14
  | DW_OP_reg15
  | DW_OP_reg16
  | DW_OP_reg17
  | DW_OP_reg18
  | DW_OP_reg19
  | DW_OP_reg20
  | DW_OP_reg21
  | DW_OP_reg22
  | DW_OP_reg23
  | DW_OP_reg24
  | DW_OP_reg25
  | DW_OP_reg26
  | DW_OP_reg27
  | DW_OP_reg28
  | DW_OP_reg29
  | DW_OP_reg30
  | DW_OP_reg31
  | DW_OP_breg0 of Int64.t
  | DW_OP_breg1 of Int64.t
  | DW_OP_breg2 of Int64.t
  | DW_OP_breg3 of Int64.t
  | DW_OP_breg4 of Int64.t
  | DW_OP_breg5 of Int64.t
  | DW_OP_breg6 of Int64.t
  | DW_OP_breg7 of Int64.t
  | DW_OP_breg8 of Int64.t
  | DW_OP_breg9 of Int64.t
  | DW_OP_breg10 of Int64.t
  | DW_OP_breg11 of Int64.t
  | DW_OP_breg12 of Int64.t
  | DW_OP_breg13 of Int64.t
  | DW_OP_breg14 of Int64.t
  | DW_OP_breg15 of Int64.t
  | DW_OP_breg16 of Int64.t
  | DW_OP_breg17 of Int64.t
  | DW_OP_breg18 of Int64.t
  | DW_OP_breg19 of Int64.t
  | DW_OP_breg20 of Int64.t
  | DW_OP_breg21 of Int64.t
  | DW_OP_breg22 of Int64.t
  | DW_OP_breg23 of Int64.t
  | DW_OP_breg24 of Int64.t
  | DW_OP_breg25 of Int64.t
  | DW_OP_breg26 of Int64.t
  | DW_OP_breg27 of Int64.t
  | DW_OP_breg28 of Int64.t
  | DW_OP_breg29 of Int64.t
  | DW_OP_breg30 of Int64.t
  | DW_OP_breg31 of Int64.t
  | DW_OP_regx of Word64.t
  | DW_OP_fbreg of Int64.t
  | DW_OP_bregx of Word64.t * Int64.t
  | DW_OP_piece of Word64.t
  | DW_OP_deref_size of Word8.t
  | DW_OP_xderef_size of Word8.t
  | DW_OP_nop
  | DW_OP_push_object_address
  | DW_OP_call2 of Word16.t
  | DW_OP_call4 of Word32.t
  | DW_OP_call_ref of Word64.t
  | DW_OP_form_tls_address
  | DW_OP_call_frame_cfa
  | DW_OP_bit_piece of Word64.t * Word64.t
  | DW_OP_user of Word64.t

let lit =
  function
  | 0x30 -> DW_OP_lit0
  | 0x31 -> DW_OP_lit1
  | 0x32 -> DW_OP_lit2
  | 0x33 -> DW_OP_lit3
  | 0x34 -> DW_OP_lit4
  | 0x35 -> DW_OP_lit5
  | 0x36 -> DW_OP_lit6
  | 0x37 -> DW_OP_lit7
  | 0x38 -> DW_OP_lit8
  | 0x39 -> DW_OP_lit9
  | 0x3a -> DW_OP_lit10
  | 0x3b -> DW_OP_lit11
  | 0x3c -> DW_OP_lit12
  | 0x3d -> DW_OP_lit13
  | 0x3e -> DW_OP_lit14
  | 0x3f -> DW_OP_lit15
  | 0x40 -> DW_OP_lit16
  | 0x41 -> DW_OP_lit17
  | 0x42 -> DW_OP_lit18
  | 0x43 -> DW_OP_lit19
  | 0x44 -> DW_OP_lit20
  | 0x45 -> DW_OP_lit21
  | 0x46 -> DW_OP_lit22
  | 0x47 -> DW_OP_lit23
  | 0x48 -> DW_OP_lit24
  | 0x49 -> DW_OP_lit25
  | 0x4a -> DW_OP_lit26
  | 0x4b -> DW_OP_lit27
  | 0x4c -> DW_OP_lit28
  | 0x4d -> DW_OP_lit29
  | 0x4e -> DW_OP_lit30
  | 0x4f -> DW_OP_lit31
  | _ -> Printf.kprintf failwith "nope"

let reg =
  function
  | 0x50 -> DW_OP_reg0
  | 0x51 -> DW_OP_reg1
  | 0x52 -> DW_OP_reg2
  | 0x53 -> DW_OP_reg3
  | 0x54 -> DW_OP_reg4
  | 0x55 -> DW_OP_reg5
  | 0x56 -> DW_OP_reg6
  | 0x57 -> DW_OP_reg7
  | 0x58 -> DW_OP_reg8
  | 0x59 -> DW_OP_reg9
  | 0x5a -> DW_OP_reg10
  | 0x5b -> DW_OP_reg11
  | 0x5c -> DW_OP_reg12
  | 0x5d -> DW_OP_reg13
  | 0x5e -> DW_OP_reg14
  | 0x5f -> DW_OP_reg15
  | 0x60 -> DW_OP_reg16
  | 0x61 -> DW_OP_reg17
  | 0x62 -> DW_OP_reg18
  | 0x63 -> DW_OP_reg19
  | 0x64 -> DW_OP_reg20
  | 0x65 -> DW_OP_reg21
  | 0x66 -> DW_OP_reg22
  | 0x67 -> DW_OP_reg23
  | 0x68 -> DW_OP_reg24
  | 0x69 -> DW_OP_reg25
  | 0x6a -> DW_OP_reg26
  | 0x6b -> DW_OP_reg27
  | 0x6c -> DW_OP_reg28
  | 0x6d -> DW_OP_reg29
  | 0x6e -> DW_OP_reg30
  | 0x6f -> DW_OP_reg31
  | _ -> Printf.kprintf failwith "nope"

let breg =
  function
  | 0x70 -> DW_OP_breg0
  | 0x71 -> DW_OP_breg1
  | 0x72 -> DW_OP_breg2
  | 0x73 -> DW_OP_breg3
  | 0x74 -> DW_OP_breg4
  | 0x75 -> DW_OP_breg5
  | 0x76 -> DW_OP_breg6
  | 0x77 -> DW_OP_breg7
  | 0x78 -> DW_OP_breg8
  | 0x79 -> DW_OP_breg9
  | 0x7a -> DW_OP_breg10
  | 0x7b -> DW_OP_breg11
  | 0x7c -> DW_OP_breg12
  | 0x7d -> DW_OP_breg13
  | 0x7e -> DW_OP_breg14
  | 0x7f -> DW_OP_breg15
  | 0x80 -> DW_OP_breg16
  | 0x81 -> DW_OP_breg17
  | 0x82 -> DW_OP_breg18
  | 0x83 -> DW_OP_breg19
  | 0x84 -> DW_OP_breg20
  | 0x85 -> DW_OP_breg21
  | 0x86 -> DW_OP_breg22
  | 0x87 -> DW_OP_breg23
  | 0x88 -> DW_OP_breg24
  | 0x89 -> DW_OP_breg25
  | 0x8a -> DW_OP_breg26
  | 0x8b -> DW_OP_breg27
  | 0x8c -> DW_OP_breg28
  | 0x8d -> DW_OP_breg29
  | 0x8e -> DW_OP_breg30
  | 0x8f -> DW_OP_breg31
  | _ -> Printf.kprintf failwith "nope"

let dw_op =
  let dw_op_lo_user = 0xe0 in
  let dw_op_hi_user = 0xff in
  function
  (*0 operand operations*)
  | 0x06 -> DW_OP_deref
  | 0x12 -> DW_OP_dup
  | 0x13 -> DW_OP_drop
  | 0x14 -> DW_OP_over
  | 0x16 -> DW_OP_swap
  | 0x17 -> DW_OP_rot
  | 0x18 -> DW_OP_xderef
  | 0x19 -> DW_OP_abs
  | 0x1a -> DW_OP_and
  | 0x1b -> DW_OP_div
  | 0x1c -> DW_OP_minus
  | 0x1d -> DW_OP_mod
  | 0x1e -> DW_OP_mul
  | 0x1f -> DW_OP_neg
  | 0x20 -> DW_OP_not
  | 0x21 -> DW_OP_or
  | 0x22 -> DW_OP_plus
  | 0x24 -> DW_OP_shl
  | 0x25 -> DW_OP_shr
  | 0x26 -> DW_OP_shra
  | 0x27 -> DW_OP_xor
  | 0x29 -> DW_OP_eq
  | 0x2a -> DW_OP_ge
  | 0x2b -> DW_OP_gt
  | 0x2c -> DW_OP_le
  | 0x2d -> DW_OP_lt
  | 0x2e -> DW_OP_ne
  | 0x96 -> DW_OP_nop
  | 0x97 -> DW_OP_push_object_address
  | 0x9b -> DW_OP_form_tls_address
  | 0x9c -> DW_OP_call_frame_cfa
  | 0x9f -> DW_OP_stack_value
  (*1 operand operations*)
  | 0x03 -> DW_OP_addr
  | 0x08 -> DW_OP_const1u
  | 0x09 -> DW_OP_const1s
  | 0x0a -> DW_OP_const2u
  | 0x0b -> DW_OP_const2s
  | 0x0c -> DW_OP_const4u
  | 0x0d -> DW_OP_const4s
  | 0x0e -> DW_OP_const8u
  | 0x0f -> DW_OP_const8s
  | 0x10 -> DW_OP_constu
  | 0x11 -> DW_OP_consts
  | 0x15 -> DW_OP_pick
  | 0x23 -> DW_OP_plus_uconst
  | 0x28 -> DW_OP_bra
  | 0x2f -> DW_OP_skip
  | 0x90 -> DW_OP_regx
  | 0x91 -> DW_OP_fbreg
  | 0x93 -> DW_OP_piece
  | 0x94 -> DW_OP_deref_size
  | 0x95 -> DW_OP_xderef_size
  | 0x98 -> DW_OP_call2
  | 0x99 -> DW_OP_call4
  | 0x9a -> DW_OP_call_ref
  (*2 operands operations*)
  | 0x9d -> DW_OP_bit_piece
  | 0x9e -> DW_OP_implicit_value
  | 0x92 -> DW_OP_bregx
  | n when n >= 0x30 && n < 0x50 -> lit n
  | n when n >= 0x50 && n < 0x70 -> reg n
  | n when n >= 0x70 && n < 0x90 -> breg n
  | n ->
    if n >= dw_op_lo_user && n <= dw_op_hi_user then
      DW_OP_user (Int64.of_int n)
    else
      Printf.kprintf failwith "unknown DW_OP %x" n
