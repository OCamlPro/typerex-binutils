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

open IntelasmTypes
open IntelasmEmit
open BinobjFile

(* TODO: I am not sure the 'estimated_pos' field is initialized somewhere,
   so maybe we should update it ? *)

(* The buffer is parametrized by the type of the buffer. During the
   assembling process, we use a Buffer.t, but we always use string in
   the external interface. *)

type 'code buffer = {
  buf : 'code;
  proc : proc_info;
  mutable labels: int option symbol StringMap.t; (* name -> position in 'buf' *)
  mutable relocations: (int * reloc_kind * string) list;
  (* jumps that have to be resolved before emitting the assembler *)
  mutable jumps : (int * instruction * string) list;
  mutable labels_estimated_pos : int StringMap.t;
  mutable short_jump_threshold : int;
  mutable shorter_jumps : int;
  mutable long_jumps : int;
}

(* A copy of Clflags.keep_asm_file to remove the dependency towards Clflags *)
(*  let keep_asm_file = ref None *)

(* When a jump has to be generated, we compare the offset between the
   source instruction and the target instruction, in number of
   instructions.

   If the offset is less than [short_jump_threshold] instructions,
   we generate a short jump during the first pass. 16 is a "safe"
   value, as most instructions are shorter than 8 bytes: [REX] +
   [OPCODE] + [MODRM] + [SIB] + [IMM32] *)

let new_buffer proc = {
  buf = Buffer.create 10000;
  proc = proc;
  labels = StringMap.empty;
  labels_estimated_pos = StringMap.empty;
  relocations = [];
  jumps = [];
  short_jump_threshold = 16;
  shorter_jumps = 0;
  long_jumps = 0;
}

let clear_buffer b =
  Buffer.clear b.buf;
  b.labels <- StringMap.empty;
  b.labels_estimated_pos <- StringMap.empty;
  b.relocations <- [];
  ()


let force_jump ins loc =
  match ins.instr with
    JMP _ ->       ins.instr <- JMP loc
  | J (_,c) ->     ins.instr <- J (loc, c)
  | _ -> ()

let is_imm32_64 n =
  let n32 = Int64.to_int32 n in
  let n64 = Int64.of_int32 n32 in
  n = n64

let is_imm32 n =
  if Sys.word_size = 4 then true else
    let n32 = Int32.of_int n in
    let n63 = Int32.to_int n32 in
    n = n63

let is_imm8 x = x < 128 && x >= -128
let is_imm8_64 x = x < 128L && x >= -128L

let rd_of_regf regf = match regf with XMM n -> n

let rd_of_reg64 reg64 = match reg64 with
  | RAX -> 0
  | RCX -> 1
  | RDX -> 2
  | RBX -> 3
  | RSP -> 4
  | RBP -> 5
  | RSI -> 6
  | RDI -> 7
  | R8  -> 8
  | R9  -> 9
  | R10 -> 10
  | R11 -> 11
  | R12 -> 12
  | R13 -> 13
  | R14 -> 14
  | R15 -> 15
  | RIP -> assert false

let rd_of_reg8 reg8 = match reg8 with
    AL -> 0
  | CL -> 1
  | DL -> 2
  | BL -> 3
  | AH -> 4
  | CH -> 5
  | DH -> 6
  | BH -> 7

  | SPL -> 4
  | BPL -> 5
  | SIL -> 6
  | DIL  -> 7
  | R8B -> 8
  | R9B -> 9
  | R10B -> 10
  | R11B -> 11
  | R12B -> 12
  | R13B -> 13
  | R14B -> 14
  | R15B -> 15

let rd_of_reg16 reg8 = match reg8 with
    AX -> 0
  | CX -> 1
  | DX -> 2
  | BX -> 3
  | SP -> 4
  | BP -> 5
  | SI -> 6
  | DI  -> 7
  | R8W -> 8
  | R9W -> 9
  | R10W -> 10
  | R11W -> 11
  | R12W -> 12
  | R13W -> 13
  | R14W -> 14
  | R15W -> 15

let rd_of_reg32 reg32 = match reg32 with
    EAX -> 0
  | ECX -> 1
  | EDX -> 2
  | EBX -> 3
  | ESP -> 4
  | EBP -> 5
  | ESI -> 6
  | EDI  -> 7

  | R8D -> 8
  | R9D -> 9
  | R10D -> 10
  | R11D -> 11
  | R12D -> 12
  | R13D -> 13
  | R14D -> 14
  | R15D -> 15


let cd_of_condition condition = match condition with
  | O -> 0
  | NO -> 1
  | B | C | NAE -> 2
  | NB | NC | AE -> 3
  | Z | E -> 4
  | NZ | NE -> 5
  | BE | NA -> 6
  | NBE | A -> 7
  | S -> 8
  | NS -> 9
  | P | PE -> 10
  | NP | PO -> 11
  | L | NGE -> 12
  | NL | GE -> 13
  | LE | NG -> 14
  | NLE | G -> 15

(* We should precompute a position for each label depending on
   the number of instructions: heuristics = offset_in_instrs x 7
*)

let rex = 0b01000000
let rexw = rex lor 0b00001000
let rexr = 0b00000100 (* extension of r *)
let rex_reg reg = if reg > 7 then rexr else 0
let rexx = 0b00000010
let rex_index reg = if reg > 7 then rexx else 0
let rexb = 0b00000001
let rex_opcode reg = if reg > 7 then rexb else 0
let rex_rm reg = if reg > 7 then rexb else 0
let rex_base reg = if reg > 7 then rexb else 0
let reg7 reg = reg land 0x07


let rex_of_reg8 rexcode reg8 = match reg8 with
    AL
  | CL
  | DL
  | BL
  | AH
  | CH
  | DH
  | BH

  | R8B
  | R9B
  | R10B
  | R11B
  | R12B
  | R13B
  | R14B
  | R15B

    -> rexcode

  | SPL
  | BPL
  | SIL
  | DIL  -> rex lor rexcode

let mod_rm_reg m rm reg =
  (m lsl 6) + (reg7 rm) + ((reg7 reg) lsl 3)

let sib scale index base =
  ((match scale with
    1 -> 0
  | 2 -> 1
  | 4 -> 2
  | 8 -> 3
  | _ -> assert false) lsl  6) lor ( (reg7 index) lsl 3 ) lor ( reg7 base )


let int64_of_constant c =
  match c with
    Constant64 iL -> iL
  | _ -> assert false

let record_reloc b kind symbol =
  b.relocations <- (Buffer.length b.buf, kind, symbol) :: b.relocations

let record_jump b ins symbol =
  b.jumps <-  (Buffer.length b.buf, ins, symbol) :: b.jumps

open LittleEndian

let buf_int8 b i = buf_int8 b.buf i
let buf_int8_64 b i = buf_int8_64 b.buf i
let buf_int16_64 b i = buf_int16_64 b.buf i
let buf_int32_64 b i = buf_int32_64 b.buf i
let buf_int64 b i = buf_int64 b.buf i

let declare_symbol b s =
  try
    StringMap.find s b.labels
  with Not_found ->
    let sym = {
      sym_name = s;
      sym_offset = None;
      sym_size = None;
      sym_global = false;
      sym_type = SymbolData;
    } in
    b.labels <- StringMap.add s sym b.labels;
    sym

let find_symbol_offset b s =
  let sym = StringMap.find s b.labels in
  match sym.sym_offset with
    None -> raise Not_found
  | Some target_pos -> target_pos

let declare_label_offset b s =
  let sym = declare_symbol b s in
  assert (sym.sym_offset = None);
  sym.sym_offset <- Some (Buffer.length b.buf)

let declare_label_global b s =
  let sym = declare_symbol b s in
  sym.sym_global <- true

let buf_opcodes b opcodes =
  List.iter (fun opcode -> buf_int8 b opcode) opcodes

let emit_rex b rexcode =
  if b.proc.proc_arch64 && rexcode <> 0 then buf_int8 b (rexcode lor rex)

let emit_mod_rm_reg b rex opcodes rm reg =
  match rm with

  | Reg rm ->
    let rm = rd_of_reg64 rm in
    emit_rex b (rex lor (rex_reg reg) lor (rex_rm rm));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b11 rm reg)

  | Reg8 reg8 ->
    let rm = rd_of_reg8 reg8 in
    emit_rex b ( (rex_of_reg8 rex reg8) lor (rex_reg reg) lor (rex_rm rm));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b11 rm reg)

  | Regf rm ->
    let rm = rd_of_regf rm in
    emit_rex b (rex lor (rex_reg reg) lor (rex_rm rm));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b11 rm reg)

    (* 64 bits memory access *)

  | Mem( (RSP|R12) as base, 1, NoBase, 0) -> (* to (%rsp|%r12) *)
    let base = rd_of_reg64 base in

    emit_rex b
      (rex lor (rex_reg reg) lor (rex_base base));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b00 base reg);
    buf_int8 b (sib 1 0b100 base);

  | Mem( (RSP|R12) as base, 1, NoBase, offset8) when is_imm8 offset8 -> (* to 0x??(%rsp) *)
    let base = rd_of_reg64 base in

    emit_rex b
      (rex lor (rex_reg reg) lor (rex_base base));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b01 0b100 reg);
    buf_int8 b (sib 1 0b100 base);
    buf_int8 b offset8

  | Mem( (RSP|R12) as base, 1, NoBase, offset32)
      when is_imm32 offset32 -> (* to 0x??(%rsp) *)
    let base = rd_of_reg64 base in

    emit_rex b
      (rex lor (rex_reg reg) lor (rex_base base));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b10 0b100 reg);
    buf_int8 b (sib 1 0b100 base);
    buf_int32_64 b (Int64.of_int offset32)

  | Mem( (RBP|R13) as base, 1, NoBase, offset8) when is_imm8 offset8 -> (* to 0x??(%rbp) *)
      (* TODO check if offset8 = 0 is enough *)
    let base = rd_of_reg64 base in

    emit_rex b
      (rex lor (rex_reg reg) lor (rex_base base));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b01 base reg);
    buf_int8 b offset8

  | Mem( rm, 1, NoBase, 0)  -> (* to 0x00(%r??) except %rsp and %rbp *)
    let rm = rd_of_reg64 rm in

    emit_rex b (rex lor (rex_reg reg) lor (rex_rm rm));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b00 rm reg);

  | Mem( rm, 1, NoBase, offset8) when is_imm8 offset8 ->
    let rm = rd_of_reg64 rm in

    emit_rex b (rex lor (rex_reg reg) lor (rex_rm rm));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b01 rm reg);
    buf_int8 b offset8

  | Mem( rm, 1, NoBase, offset32) when is_imm32 offset32 ->
    let rm = rd_of_reg64 rm in

    emit_rex b (rex lor (rex_reg reg) lor (rex_rm rm));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b10 rm reg);
    buf_int32_64 b (Int64.of_int offset32)

  | Mem (RIP, 1, BaseSymbol symbol, 0) when b.proc.proc_arch64 ->

    emit_rex b (rex lor (rex_reg reg));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b00 0b101 reg);
      (*
        1b3:   f2 0f 10 05 1f 00 00    movsd  0x1f,%xmm0
        record_reloc b (RELOC_REL32 None) symbol; (* was RELOC_RIP32 *)
      *)
    record_reloc b RELOC_RIP32 symbol;
    buf_int32_64 b 0L

  | Mem( index, scale, NoBase, offset)  ->
    let index = rd_of_reg64 index in

    emit_rex b (rex lor (rex_reg reg) lor (rex_index index) );
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b00 0b100 reg);
    buf_int8 b (sib scale index 0b101);
    buf_int32_64 b (Int64.of_int offset)


  | Mem(index, scale, BaseReg ( (RBP|R13) as base ), 0) -> (* to 0x00(%rbp+reg) *)
    let index = rd_of_reg64 index in
    let base = rd_of_reg64 base in

    emit_rex b
      (rex lor (rex_reg reg) lor (rex_index index) lor (rex_base base));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b01 0b100 reg);
    buf_int8 b (sib scale index base);
    buf_int8 b 0

  | Mem( index, scale, BaseReg base, 0)  ->
    let index = rd_of_reg64 index in
    let base = rd_of_reg64 base in

    emit_rex b
      (rex lor (rex_reg reg) lor (rex_index index) lor (rex_base base));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b00 0b100 reg);
    buf_int8 b (sib scale index base)


  | Mem( index, scale, BaseReg base, offset) when is_imm8 offset ->
    let index = rd_of_reg64 index in
    let base = rd_of_reg64 base in

    emit_rex b
      (rex lor (rex_reg reg) lor (rex_index index) lor (rex_base base));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b01 0b100 reg);
    buf_int8 b (sib scale index base);
    buf_int8 b offset

  | Mem( index, scale, BaseReg base, offset) when is_imm32 offset ->
    let index = rd_of_reg64 index in
    let base = rd_of_reg64 base in

    emit_rex b
      (rex lor (rex_reg reg) lor (rex_index index) lor (rex_base base));
    buf_opcodes b opcodes;
    buf_int8 b (mod_rm_reg 0b10 0b100 reg);
    buf_int8 b (sib scale index base);
    buf_int32_64 b (Int64.of_int offset)

  | _ -> raise Not_found

let emit_movlpd b dst src =
  match dst, src with
  | Regf reg, (Mem _  as rm) ->
    buf_int8 b 0x66;
    emit_mod_rm_reg b 0 [ 0x0f; 0x12 ] rm (rd_of_regf reg)
  | (Mem _  as rm), Regf reg ->
    buf_int8 b 0x66;
    emit_mod_rm_reg b 0 [ 0x0f; 0x13 ] rm (rd_of_regf reg)
  | _ -> raise Not_found

let emit_movapd b dst src =
  match dst, src with
  | Regf reg, (Regf _ | Mem _  as rm) ->
    buf_int8 b 0x66;
    emit_mod_rm_reg b 0 [ 0x0f; 0x28 ] rm (rd_of_regf reg)
  | (Mem _  as rm), Regf reg ->
    buf_int8 b 0x66;
    emit_mod_rm_reg b 0 [ 0x0f; 0x29 ] rm (rd_of_regf reg)
  | _ -> raise Not_found

let emit_movsd b dst src =
  match dst, src with
  | Regf reg, (Regf _ | Mem _ as rm) ->
    buf_int8 b 0xF2;
    emit_mod_rm_reg b 0 [ 0x0f; 0x10 ] rm (rd_of_regf reg)
  | (Mem _ as rm), Regf reg ->
    buf_int8 b 0xF2;
    emit_mod_rm_reg b 0 [ 0x0f; 0x11 ] rm (rd_of_regf reg)
  | _ -> raise Not_found

let emit_movss b dst src =
  match dst, src with
  | Regf reg, (Regf _ | Mem _  as rm) ->
    buf_int8 b 0xF3;
    emit_mod_rm_reg b 0 [ 0x0f; 0x10 ] rm (rd_of_regf reg)
  | (Mem _  as rm), Regf reg ->
    buf_int8 b 0xF3;
    emit_mod_rm_reg b 0 [ 0x0f; 0x11 ] rm (rd_of_regf reg)
  | _ -> raise Not_found

let emit_andpd b dst src =
  match dst, src with
  | Regf reg, (Regf _ | Mem _  as rm) ->
    buf_int8 b 0x66;
    emit_mod_rm_reg b 0 [ 0x0f; 0x54 ] rm (rd_of_regf reg)
  | _ -> raise Not_found

let imm8_of_rounding rounding =
  (* Precision Mask = Normal instead of Inexact *)
  (* Rounding Select = imm8.RC instead of MXCSR.RC *)
  match rounding with
  | RoundNearest -> 0b00
  | RoundDown -> 0b01
  | RoundUp -> 0x10
  | RoundTruncate -> 0x11

let emit_roundsd b dst rounding src =
  let rounding = imm8_of_rounding rounding in
  match dst, src with
  | Regf reg, (Regf _ | Mem _ as rm) ->
    buf_int8 b 0x66;
    emit_mod_rm_reg b 0 [ 0x0f; 0x3A; 0x0B ] rm (rd_of_regf reg);
    buf_int8 b rounding
  | _ -> raise Not_found

let emit_addsd b dst src =
  match dst, src with
  | Regf reg, (Regf _ | Mem _  as rm) ->
    buf_int8 b 0xF2;
    emit_mod_rm_reg b 0 [ 0x0f; 0x58 ] rm (rd_of_regf reg)
  | _ -> raise Not_found

let emit_sqrtsd b dst src =
  match dst, src with
  | Regf reg, (Regf _ | Mem _  as rm) ->
    buf_int8 b 0xF2;
    emit_mod_rm_reg b 0 [ 0x0f; 0x51 ] rm (rd_of_regf reg)
  | _ -> raise Not_found

let emit_mulsd b dst src =
  match dst, src with
  | Regf reg, (Regf _ | Mem _  as rm) ->
    buf_int8 b 0xF2;
    emit_mod_rm_reg b 0 [ 0x0f; 0x59 ] rm (rd_of_regf reg)
  | _ -> raise Not_found

let emit_divsd b dst src =
  match dst, src with
  | Regf reg, (Regf _ | Mem _  as rm) ->
    buf_int8 b 0xF2;
    emit_mod_rm_reg b 0 [ 0x0f; 0x5E ] rm (rd_of_regf reg)
  | _ -> raise Not_found

let emit_subsd b dst src =
  match dst, src with
  | Regf reg, (Regf _ | Mem _  as rm) ->
    buf_int8 b 0xF2;
    emit_mod_rm_reg b 0 [ 0x0f; 0x5C ] rm (rd_of_regf reg)
  | _ -> raise Not_found

let emit_xorpd b dst src =
  match dst, src with
  | Regf reg, (Regf _ | Mem _  as rm) ->
    buf_int8 b 0x66;
    emit_mod_rm_reg b 0 [ 0x0f; 0x57 ] rm (rd_of_regf reg)
  | _ -> raise Not_found

let emit_cvtsi2sd b dst src =
  match dst, src with
  | Regf reg, (Reg _ | Mem _  as rm) ->
    buf_int8 b 0xF2;
    emit_mod_rm_reg b rexw [ 0x0f; 0x2A ] rm (rd_of_regf reg)
  | _ -> raise Not_found

let emit_cvtsd2si b dst src =
  match dst, src with
  | Reg reg, (Regf _ | Mem _  as rm) ->
    buf_int8 b 0xF2;
    emit_mod_rm_reg b rexw [ 0x0f; 0x2D ] rm (rd_of_reg64 reg)
  | _ -> raise Not_found

let emit_cvttsd2si b dst src =
  match dst, src with
  | Reg reg, (Regf _ | Mem _  as rm) ->
    buf_int8 b 0xF2;
    emit_mod_rm_reg b rexw [ 0x0f; 0x2C ] rm (rd_of_reg64 reg)
  | _ -> raise Not_found

let emit_cvtsd2ss b dst src =
  match dst, src with
  | Regf reg, (Regf _ | Mem _  as rm) ->
    buf_int8 b 0xF2;
    emit_mod_rm_reg b rexw [ 0x0f; 0x5A ] rm (rd_of_regf reg)
  | _ -> raise Not_found

let emit_cvtss2sd b dst src =
  match dst, src with
  | Regf reg, (Regf _ | Mem _  as rm) ->
    buf_int8 b 0xF3;
    emit_mod_rm_reg b rexw [ 0x0f; 0x2A ] rm (rd_of_regf reg)
  | _ -> raise Not_found

let emit_comisd b dst src =
  match dst, src with
  | Regf reg, (Regf _ | Mem _  as rm) ->
    buf_int8 b 0x66;
    emit_mod_rm_reg b 0 [ 0x0f; 0x2F ] rm (rd_of_regf reg)
  | _ -> raise Not_found

let emit_ucomisd b dst src =
  match dst, src with
  | Regf reg, (Regf _ | Mem _  as rm) ->
    buf_int8 b 0x66;
    emit_mod_rm_reg b 0 [ 0x0f; 0x2E ] rm (rd_of_regf reg)
  | _ -> raise Not_found

let emit_movb b dst src =
  match dst, src with
  | (Mem _  as rm) , Reg8 reg ->
    emit_mod_rm_reg b (rex_of_reg8 0 reg) [ 0x88 ] rm (rd_of_reg8 reg); (* no REX.W *)

  | _ -> raise Not_found

let emit_movw b dst src =
  match dst, src with
  | (Mem _  as rm) , Reg16 reg ->
    buf_int8 b 0x66;
    emit_mod_rm_reg b rex [ 0x89 ] rm (rd_of_reg16 reg) (* no REX.W *)
  | _ -> raise Not_found

let emit_movl b dst src =
  match dst, src with
  | Reg32 reg32, (Mem _  as rm)  ->
    let reg = rd_of_reg32 reg32 in
    emit_mod_rm_reg b 0 [ 0x8B ] rm reg
  | (Mem _  as rm), Reg32 reg32 ->
    let reg = rd_of_reg32 reg32 in
    emit_mod_rm_reg b 0 [ 0x89 ] rm reg
  | (Mem _  as rm), Constant64 iL when is_imm32_64 iL ->
    let reg = 0 in
    emit_mod_rm_reg b 0 [ 0xC7 ] rm reg;
    buf_int32_64 b iL
  | Reg32 r32, Constant64 iL ->
    let reg = rd_of_reg32 r32 in
    buf_int8 b (0xB8 lor (reg7 reg) );
    buf_int32_64 b iL
  | _ -> raise Not_found

let emit_mov b dst src =
  match dst, src with
  | Reg reg, ((Reg _ | Mem _ ) as rm) ->
    emit_mod_rm_reg b rexw [ 0x8B ] rm (rd_of_reg64 reg)
  | (Mem _  as rm) , Reg reg ->
    emit_mod_rm_reg b rexw [ 0x89 ] rm (rd_of_reg64 reg)
  | (Mem _ | Reg _) as rm, Constant64 iL when is_imm32_64 iL ->
    emit_mod_rm_reg b rexw [ 0xC7 ] rm 0;
    buf_int32_64 b iL
  | Reg r64, Constant64 iL when b.proc.proc_arch64 -> (* MOVABSQ *)
    let reg = rd_of_reg64 r64 in
    emit_rex b (rexw lor (rex_opcode reg) );
    buf_int8 b (0xB8 lor (reg7 reg) );
    buf_int64 b iL
  | _ -> raise Not_found

type encoding =
  {
    rax_imm32 : int list option;
    r64_rm64 : int list option;
    rm64_r64 : int list option;
    rm64_imm8 : (int list * int) option;
    rm64_imm32 : (int list * int) option;
  }

let emit_encoding enc b dst src =
  match enc, dst, src with

    (* 64 bits encodings *)

  | { r64_rm64 = Some opcodes}, Reg reg, ((Reg _ | Mem _ ) as rm) ->
    emit_mod_rm_reg b rexw opcodes rm (rd_of_reg64 reg)

  | { rm64_r64 = Some opcodes}, (Reg _ | Mem _  as rm) , Reg reg ->
    emit_mod_rm_reg b rexw opcodes rm (rd_of_reg64 reg)

  | { rm64_imm8 = Some (opcodes, reg) },
    (Reg _ | Mem _  as rm), Constant64 n when is_imm8_64 n ->
    emit_mod_rm_reg b rexw opcodes rm reg;
      buf_int8_64 b n

  | { rax_imm32 = Some opcodes },
      Reg RAX, Constant64 n when is_imm32_64 n ->
    emit_rex b rexw;
        buf_opcodes b opcodes;
        buf_int32_64 b n

  | { rm64_imm32 = Some (opcodes, reg) },
        (Reg _ | Mem _  as rm), Constant64 n when is_imm32_64 n ->
    emit_mod_rm_reg b rexw opcodes rm reg;
          buf_int32_64 b n

  | { rm64_imm32 = Some (opcodes, reg) },
          (Reg _ | Mem _  as rm),
          LabelRel (symbol, offset) ->
    emit_mod_rm_reg b rexw opcodes rm reg;
            record_reloc b RELOC_REL32 symbol;
            buf_int32_64 b offset

  | _ -> raise Not_found

let emit_add = emit_encoding {
  rax_imm32 = Some [0x05];
  r64_rm64 = Some [0x03];
  rm64_r64 = Some [0x01];
  rm64_imm8 = Some ([0x83], 0);
  rm64_imm32 = Some ([0x81], 0);
}

let emit_or = emit_encoding {
  rax_imm32 = Some [0x0D];
  r64_rm64 = Some [0x0B];
  rm64_r64 = Some [0x09];
  rm64_imm8 = Some ([0x83], 1);
  rm64_imm32 = Some ([0x81], 1);
}

let emit_and = emit_encoding {
  rax_imm32 = Some [0x25];
  r64_rm64 = Some [0x23];
  rm64_r64 = Some [0x21];
  rm64_imm8 = Some ([0x83], 4);
  rm64_imm32 = Some ([0x81], 4);
}

let emit_sub = emit_encoding {
  rax_imm32 = Some [0x2D];
  r64_rm64 = Some [0x2B];
  rm64_r64 = Some [0x29];
  rm64_imm8 = Some ([0x83], 5);
  rm64_imm32 = Some ([0x81], 5);
}

let emit_xor = emit_encoding {
  rax_imm32 = Some [0x35];
  r64_rm64 = Some [0x33];
  rm64_r64 = Some [0x31];
  rm64_imm8 = Some ([0x83], 6);
  rm64_imm32 = Some ([0x81], 6);
}

let emit_cmp = emit_encoding {
  rax_imm32 = Some [0x3D];
  r64_rm64 = Some [0x3B];
  rm64_r64 = Some [0x39];
  rm64_imm8 = Some ([0x83], 7);
  rm64_imm32 = Some ([0x81], 7);
}

let emit_testq b dst src =
  match dst, src with
  | (Reg _ | Mem _ ) as rm, Reg reg ->
    let reg = rd_of_reg64 reg in
    emit_mod_rm_reg b rexw [ 0x85 ] rm reg
  | Reg RAX, Constant64 iL when is_imm32_64 iL ->
    emit_rex b rexw;
    buf_opcodes b [ 0xA9 ];
    buf_int32_64 b iL
  | (Reg _ | Mem _ ) as rm, Constant64 iL when is_imm32_64 iL ->
    emit_mod_rm_reg b rexw [ 0xF7 ] rm 0;
    buf_int32_64 b iL
  | Reg8 AL, Constant64 iL when is_imm8_64 iL ->
    buf_opcodes b [ 0xA8 ];
    buf_int8_64 b iL
  | Reg8 _ as rm, Constant64 iL when is_imm8_64 iL ->
    emit_mod_rm_reg b 0 [ 0xF6 ] rm 0;
    buf_int8_64 b iL
  | _ -> raise Not_found

let emit_imulq b dst src =
  match dst, src with
    Reg reg, (Reg _ | Mem _  as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [0x0F; 0xAF] rm reg
  | (Reg reg as rm), Constant64 iL when is_imm8_64 iL ->
    let reg = rd_of_reg64 reg in
    emit_mod_rm_reg b rexw [0x6B] rm reg;
    buf_int8_64 b iL
  | (Reg reg as rm), Constant64 iL when is_imm32_64 iL ->
    let reg = rd_of_reg64 reg in
    emit_mod_rm_reg b rexw [0x69] rm reg;
    buf_int32_64 b iL
  | _ -> assert false

let emit_idivq b dst =
  let reg = 7 in
  match dst with
    Reg _ | Mem _  as rm ->
      emit_mod_rm_reg b rexw [0xF7] rm reg
  | _ -> assert false

let emit_shift reg b dst src =
  match dst, src with
  | Reg _ as rm, Constant64 1L ->
    emit_mod_rm_reg b rexw [ 0xD1 ] rm reg

  | Reg _ as rm, Constant64 n ->
    emit_mod_rm_reg b rexw [ 0xC1 ] rm reg;
    buf_int8_64 b n

  | Reg _ as rm, Reg8 CL ->
    emit_mod_rm_reg b rexw [ 0xD3 ] rm reg
  | _ -> raise Not_found

let emit_salq b dst src =
  let reg = 4 in
  emit_shift reg b dst src

let emit_shrq b dst src =
  let reg = 5 in
  emit_shift reg b dst src

let emit_sarq b dst src =
  let reg = 7 in
  emit_shift reg b dst src

let emit_jmp b loc ins dst =
  match dst with
    LabelRel (symbol, offset) -> begin
      try
        let target_pos = find_symbol_offset b symbol in
        let source_pos = Buffer.length b.buf
          + 2 (* OPCODE + OFFSET/imm8 *) in
        let offset = target_pos - source_pos in
        if offset >= -128 && offset < 128 then begin
          buf_int8 b 0xEB;
          buf_int8 b offset;
        end else
          let offset = offset - 3 in (* imm8 -> imm32 = +3 bytes *)
          buf_int8 b 0xE9;
          buf_int32_64 b (Int64.of_int offset);
      with Not_found ->
        let near = try
                     match loc with
                       Loc_unknown ->
                         let est_pos = StringMap.find symbol b.labels_estimated_pos
                         in
                         let distance = abs (ins.pos - est_pos) in
                         distance < b.short_jump_threshold
                     | Loc_near -> true
                     | Loc_far -> false
          with Not_found -> false
        in
        if near then begin
            (* This is a bit conservative, but it should work *)
          buf_int8 b 0xEB;
          force_jump ins Loc_near;
          record_reloc b RELOC_REL8 symbol;
          assert (offset = 0L);
          buf_int8_64 b offset
        end else begin
          buf_int8 b 0xE9;
          force_jump ins Loc_far;
          record_jump b ins symbol;
            (*                record_reloc b (RELOC_REL32 (Some ins)) symbol; *)
          buf_int32_64 b offset
        end
    end

  | Reg _ | Mem _ as rm ->
    let reg = 4 in
    emit_mod_rm_reg b 0 [0xFF] rm reg (* no REX *)

    (*
      | Mem _ as rm ->
      let reg = 4 in
      emit_mod_rm_reg b rex [0xFF] rm reg (* no REX.W *)
    *)

  | _ -> raise Not_found

let emit_call b dst =
  match dst with
    LabelRel (symbol, offset) ->
      buf_int8 b 0xE8;
      record_reloc b RELOC_REL32 symbol;
      buf_int32_64 b offset

  | Reg _ | Mem _ as rm ->
    let reg = 2 in
    emit_mod_rm_reg b 0 [0xFF] rm reg (* no REX *)

  | _ -> raise Not_found

let emit_j b loc ins condition dst =
  match dst with
    LabelRel (symbol, offset) -> begin

      try

        let target_pos = find_symbol_offset b symbol in
        let source_pos = Buffer.length b.buf
          + 2 (* OPCODE + OFFSET/imm8 *) in
        let offset = target_pos - source_pos in
        if abs offset < 128 then begin
          buf_int8 b (0x70 + cd_of_condition condition);
          buf_int8 b offset;
        end else
          let offset = offset - 4 in (*
                                       prefix opcode = +1
                                       imm8 -> imm32 = +3 bytes *)
          buf_int8 b 0x0F;
          buf_int8 b (0x80 + cd_of_condition condition);
          buf_int32_64 b (Int64.of_int offset);
      with Not_found ->
        let near =
          match loc with
            Loc_unknown ->
              let est_pos = StringMap.find symbol b.labels_estimated_pos
              in
              let distance = abs (ins.pos - est_pos) in
              distance < b.short_jump_threshold
          | Loc_near -> true
          | Loc_far -> false
        in

        if near then begin
            (* This is a bit conservative, but it should work *)
          buf_int8 b (0x70 + cd_of_condition condition);
          force_jump ins Loc_near;
          record_reloc b RELOC_REL8 symbol;
          assert(offset = 0L);
          buf_int8_64 b offset
        end else begin
          buf_int8 b 0x0F;
          buf_int8 b (0x80 + cd_of_condition condition);
          force_jump ins Loc_far;
          record_jump b ins symbol;
          buf_int32_64 b offset
        end
    end

  | _ -> raise Not_found

let emit_cmov b condition dst src =
  match dst, src with
  | Reg reg, (Reg _ | Mem _ as rm) ->
    emit_mod_rm_reg b rexw [ 0x0F; 0x40 + cd_of_condition condition ] rm (rd_of_reg64 reg)

  | _ -> raise Not_found

let emit_set b condition dst =
  match dst with
    Reg8 _ as rm ->
      emit_mod_rm_reg b 0 [0x0F; 0x90 + cd_of_condition condition] rm 0
  | _ -> raise Not_found

let emit_movsx_byte b dst src =
  match dst, src with
    Reg reg, (Mem _ | Reg8 _ as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rex [0x0F;0xBE] rm reg (* no REX.W *)
  | _ -> raise Not_found

let emit_movsx_word b dst src =
  match dst, src with
    Reg reg, (Mem _ | Reg16 _ as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [0x0F;0xBF] rm reg
  | _ -> raise Not_found

let emit_movsxd b dst src =
  match dst, src with
    Reg reg, (Mem _ | Reg32 _ as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [0x63] rm reg
  | _ -> raise Not_found

let emit_movzx_byte b dst src =
  match dst, src with
    Reg reg, (Mem _ | Reg8 _ as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [0x0F;0xB6] rm reg
  | _ -> raise Not_found

let emit_movzx_word b dst src =
  match dst, src with
    Reg reg, (Mem _ as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [0x0F;0xB7] rm reg
  | _ -> raise Not_found

let emit_fstpl b dst =
  match dst with
    (Mem _ as rm) ->
      let reg = 3 in
      emit_mod_rm_reg b 0 [0xDD] rm reg
  | _ -> raise Not_found



let emit_neg b dst =
  match dst with
    ( (Reg _|Mem _) as rm) ->
      let reg = 3 in
      emit_mod_rm_reg b rexw [ 0xF7 ] rm reg

  | _ -> raise Not_found

let emit_leaq b dst src =
  match dst, src with
    Reg reg, (Mem _ as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x8D ] rm reg

  | _ -> raise Not_found

let emit_stack b opcode dst =
  match dst with
    Reg reg ->
      let reg = rd_of_reg64 reg in
      if reg > 7 then
        emit_rex b (rex lor (rex_opcode reg));
      buf_int8 b (opcode + reg7 reg);
  | Reg32 reg ->
    let reg = rd_of_reg32 reg in
    buf_int8 b (opcode + reg7 reg);
  | _ -> assert false

let emit_push b dst =
  emit_stack b 0x50 dst

let emit_pop b dst =
  emit_stack b 0x58 dst

let emit_leave b =
  buf_int8 b 0xC9

let emit_incq b dst =
  match dst with
    Reg _ | Mem _  as rm ->
      let reg = 0 in
      emit_mod_rm_reg b rexw [0xFF] rm reg
  | _ -> assert false

let emit_decq b dst =
  match dst with
    Reg _ | Mem _  as rm ->
      let reg = 1 in
      emit_mod_rm_reg b rexw [0xFF] rm reg
  | _ -> assert false

let emit_ret b =
  buf_int8 b 0xC3

let emit_cqto b =
  emit_rex b rexw; (* REX.W *)
  buf_int8 b 0x99

let emit_xchg b dst src =
  match dst, src with
  | Reg8 dst, Reg8 src ->
    buf_int8 b 0x86;
    buf_int8 b (0xc4 lor (rd_of_reg8 src lsl 3) lor (rd_of_reg8 dst))
  | _ ->
    raise Not_found

let emit_bswap b dst =
  match dst with
    Reg reg ->
      let rd = rd_of_reg64 reg in
      emit_rex b (rex lor (rex_rm rd) lor rexw);
      buf_int8 b 0x0F;
      buf_int8 b (0xC8 + reg7 rd)
  | Reg32 reg ->
    let rd = rd_of_reg32 reg in
    if rd > 7
    then emit_rex b (rex lor (rex_reg rd));
    buf_int8 b 0x0F;
    buf_int8 b (0xC8 + reg7 rd)
  | _ -> assert false

let asse_instr b ins =
  (*      Printf.fprintf stderr "%d: %s\n" (Buffer.length b.buf) (Amd64_emit_print.string_of_instr ins); *)
  try
    match ins.instr, ins.args with
      Global s, _ ->
        declare_label_global b s
    | Byte, [ Constant64 iL ] -> buf_int8_64 b iL
    | Byte, _ -> assert false
    | Comment _, _ -> ()

    | Word, [ Constant64 iL ] ->  buf_int16_64 b iL
    | Word, _ -> assert false

    | Dword, [ Constant64 iL ] -> buf_int32_64 b iL
    | Dword, [ LabelRel (symbol, offset) ] ->
      record_reloc b RELOC_REL32 symbol;
      buf_int32_64 b offset
    | Dword, _ -> assert false

    | Qword, [ Constant64 iL ] -> buf_int64 b iL
    | Qword, [ LabelAbs(symbol, offset) ] ->
      record_reloc b RELOC_DIR64 symbol;
      buf_int64 b offset
    | Qword, _ -> assert false

    | NewLabel s, _ ->
        (* TO BE REMOVED b.globals <- StringSet.add s b.globals; *)
      declare_label_offset b s
    | Bytes s, _ ->
      Buffer.add_string b.buf s
    | Align (data,n), _ ->
      let pos = Buffer.length b.buf in
      let current = pos mod n in
      if current > 0 then
        let n = n - current in
        if data then
          for i = 1 to n do
            buf_int8 b 0x00
          done
        else
          begin
            match n with
            | 0 -> ()
            | 1 ->
              buf_int8 b 0x90;
            | 2 ->
              buf_int8 b 0x66;
              buf_int8 b 0x90;
            | 3 ->
              buf_int8 b 0x0f;
              buf_int8 b 0x1f;
              buf_int8 b 0x00;
            | 4 ->
              buf_int8 b 0x0f;
              buf_int8 b 0x1f;
              buf_int8 b 0x40;
              buf_int8 b 0x00;
            | 5 ->
              buf_int8 b 0x0f;
              buf_int8 b 0x1f;
              buf_int8 b 0x44;
              buf_int8 b 0x00;
              buf_int8 b 0x00;
            | 6 ->
              buf_int8 b 0x66;
              buf_int8 b 0x0f;
              buf_int8 b 0x1f;
              buf_int8 b 0x44;
              buf_int16_64 b 0L
            | 7 ->
              buf_int8 b 0x0f;
              buf_int8 b 0x1f;
              buf_int8 b 0x80;
              buf_int32_64 b 0L

            | _ ->
              for i = 9 to n do
                buf_int8 b 0x66;
              done;
              buf_int8 b 0x0f;
              buf_int8 b 0x1f;
              buf_int8 b 0x84;
              buf_int8 b 0x00;
              buf_int32_64 b 0L
          end

    | MOVB, [dst; src] -> emit_movb b dst src
    | MOVB, _ -> assert false

    | MOVW, [dst; src] -> emit_movw b dst src
    | MOVW, _ -> assert false


    | MOV, [dst; src] -> emit_mov b dst src
    | MOV, _ -> assert false

    | SALQ, [dst; src] -> emit_salq b dst src
    | SALQ, _ -> assert false

    | SARQ, [dst; src] -> emit_sarq b dst src
    | SARQ, _ -> assert false

    | SHRQ, [dst; src] -> emit_shrq b dst src
    | SHRQ, _ -> assert false

    | FSTPL, [dst] -> emit_fstpl b dst
    | FSTPL, _ -> assert false

    | MOVAPD, [dst; src] -> emit_movapd b dst src
    | MOVAPD, _ -> assert false

    | MOVLPD, [dst; src] -> emit_movlpd b dst src
    | MOVLPD, _ -> assert false

    | MOVSD, [dst; src] -> emit_movsd b dst src
    | MOVSD, _ -> assert false

    | MOVSS, [dst; src] -> emit_movss b dst src
    | MOVSS, _ -> assert false

    | XORPD, [dst; src] -> emit_xorpd b dst src
    | XORPD, _ -> assert false

    | ANDPD, [dst; src] -> emit_andpd b dst src
    | ANDPD, _ -> assert false

    | SUBSD, [dst; src] -> emit_subsd b dst src
    | SUBSD, _ -> assert false

    | NEG, [dst] -> emit_neg b dst
    | NEG, _ -> assert false

    | ADDSD, [dst; src] -> emit_addsd b dst src
    | ADDSD, _ -> assert false

    | SQRTSD, [dst; src] -> emit_sqrtsd b dst src
    | SQRTSD, _ -> assert false

    | ROUNDSD rounding, [dst; src] -> emit_roundsd b dst rounding src
    | ROUNDSD _, _ -> assert false

    | MULSD, [dst; src] -> emit_mulsd b dst src
    | MULSD, _ -> assert false

    | DIVSD, [dst; src] -> emit_divsd b dst src
    | DIVSD, _ -> assert false

    | CVTSI2SD, [dst; src] -> emit_cvtsi2sd b dst src
    | CVTSI2SD, _ -> assert false

    | CVTSD2SI, [dst; src] -> emit_cvtsd2si b dst src
    | CVTSD2SI, _ -> assert false

    | CVTTSD2SI, [dst; src] -> emit_cvttsd2si b dst src
    | CVTTSD2SI, _ -> assert false

    | CVTSD2SS , [dst; src] -> emit_cvtsd2ss b dst src
    | CVTSD2SS,  _ -> assert false

    | CVTSS2SD, [dst; src] -> emit_cvtss2sd b dst src
    | CVTSS2SD, _ -> assert false

    | COMISD, [dst; src] -> emit_comisd b dst src
    | COMISD, _ -> assert false

    | UCOMISD, [dst; src] -> emit_ucomisd b dst src
    | UCOMISD, _ -> assert false


    | LEAVE, [] -> emit_leave b
    | LEAVE, _ -> assert false

    | ADD, [dst; src] -> emit_add b dst src
    | ADD, _ -> assert false

    | IMULQ, [dst; src] -> emit_imulq b dst src
    | IMULQ, _ -> assert false

    | IDIVQ, [dst] -> emit_idivq b dst
    | IDIVQ, _ -> assert false

    | SUB, [dst; src] -> emit_sub b dst src
    | SUB, _ -> assert false

    | OR, [dst; src] -> emit_or b dst src
    | OR, _ -> assert false

    | XOR, [dst; src] -> emit_xor b dst src
    | XOR, _ -> assert false

    | AND, [dst; src] -> emit_and b dst src
    | AND, _ -> assert false

    | TESTQ, [dst; src] -> emit_testq b dst src
    | TESTQ, _ -> assert false

    | CQTO, [] ->  emit_cqto b
    | CQTO, _ -> assert false

    | RET, [] -> emit_ret b
    | RET, _ -> assert false

    | CMP, [dst; src] -> emit_cmp b dst src
    | CMP, _ -> assert false

    | CMOV condition, [dst; src] -> emit_cmov b condition dst src
    | CMOV _, _ -> assert false

    | LEAQ, [dst; src] -> emit_leaq b dst src
    | LEAQ, _ -> assert false

    | MOVSX_byte, [ dst; src ] -> emit_movsx_byte b dst src
    | MOVSX_byte, _ -> assert false

    | MOVSX_word, [ dst; src ] -> emit_movsx_word b dst src
    | MOVSX_word, _ -> assert false

    | MOVSXD, [ dst; src ] -> emit_movsxd b dst src
    | MOVSXD, _ -> assert false

    | MOVZX_byte, [ dst; src ] -> emit_movzx_byte b dst src
    | MOVZX_byte, _ -> assert false

    | MOVZX_word, [ dst; src ] -> emit_movzx_word b dst src
    | MOVZX_word, _ -> assert false

    | J (loc,condition), [ dst ] -> emit_j b loc ins condition dst
    | J _, _ -> assert false

    | SET condition, [ dst ] -> emit_set b condition dst
    | SET _, _ -> assert false

    | JMP loc, [ dst ] -> emit_jmp b loc ins dst
    | JMP _, _ -> assert false

    | CALL, [ dst] -> emit_call b dst
    | CALL, _ -> assert false

    | PUSH, [ dst ] -> emit_push b dst
    | PUSH, _ -> assert false

    | POP, [ dst ] -> emit_pop b dst
    | POP, _ -> assert false

    | INCQ, [ dst ] -> emit_incq b dst
    | INCQ, _ -> assert false

    | DECQ, [ dst ] -> emit_decq b dst
    | DECQ, _ -> assert false

    | MOVL, [dst; src] -> emit_movl b dst src
    | MOVL, _ -> assert false

    | XCHG, [dst; src] -> emit_xchg b dst src
    | XCHG, _ -> assert false

    | BSWAP, [dst] -> emit_bswap b dst
    | BSWAP, _ -> assert false

  with e ->
    Printf.fprintf stderr "Exception %s: %s\n%!"
      (Printexc.to_string e)
      (IntelasmPrinter.string_of_instr b.proc ins);
    exit 2

let rec asse_instrs b instrs =
  match instrs with
    [] -> ()
  | instr :: instrs ->
    asse_instr b instr;
    asse_instrs b instrs


  (*
    (* TODO: use of virtual emitter based on functor *)

    end (* end of Emitter functor *)

    module StringEmitter = Emitter(Buffer)
    module VirtualEmitter = Emitter(struct

    type t = int ref
    let add_char t c = incr t
    let add_string t s = t := !t + String.length s
    let length t = !t

    end)

    open StringEmitter
  *)



let assemble_instrs proc instrs labels_estimated_pos =

  let text = new_buffer proc in
  text.labels_estimated_pos <- labels_estimated_pos;
  Queue.iter (fun instr -> asse_instr text instr) instrs;
  { text with buf = Buffer.to_bytes text.buf }

  (* [resolve_short_jumps] will replace internal relocations
     (corresponding to internal branchments) by their correct value,
     saving some time for the linker. It will also compute if a long
     jump can be replace by a short jump, in which case it will modify
     the location of the original symbolic instruction by a Loc_near
     location, forcing a short jump next time the symbolic code is
     re-assembled.
  *)
let resolve_short_jumps b =
    (*  Printf.fprintf stderr "IntelasmAssembler.relocate_segment\n%!"; *)
  let data = b.buf in
  List.iter
    (fun (pos, ins, s) ->
      try
        let source_pos = pos + 4 in
        let target_pos = find_symbol_offset b s in
        let n = target_pos - source_pos in
        let new_loc =
            (* Don't use 128, as the distance may still vary by about 16
               bytes depending on the alignments: the code might be
               shorter, but the source might have moved by -15 due to a
               better alignment while the target didn't move because of
               its alignment.  *)
          if n >= -110 && n < 110 then begin
            b.shorter_jumps <- b.shorter_jumps + 1;
            Loc_near
          end else begin
            b.long_jumps <- b.long_jumps + 1;
            Loc_far
          end
        in
        force_jump ins new_loc;
        let offset, _ = LittleEndian.Byt.get_int32 data pos in
        let togo = Int32.add (Int32.of_int n) offset in
        LittleEndian.str_int32 data pos togo;
      with Not_found ->
        b.relocations <- (pos, RELOC_REL32, s) :: b.relocations
    )
    b.jumps;
  b.jumps <- []


  (* [assemble_segment] generates an BinobjFile.segment from and
     IntelasmEmit.segment, i.e. translating instructions into
     binary code. *)
  (* It does a recursion until least than 5 jumps can be optimized
     to shorter jumps. Maybe we could find a better algorithm to do
     that ?
  *)


let assemble_segment proc text_seg =
  let rec iter text_seg =
    let labels_estimated_pos = text_seg.seg_labels in
    let buf = assemble_instrs proc text_seg.seg_instrs labels_estimated_pos in
    resolve_short_jumps buf;
    if buf.shorter_jumps > 5 then
      iter text_seg
    else
      let seg_symbols = StringMap.map (fun sym ->
        let sym_offset = match sym.sym_offset with
            None -> assert false (* TODO *)
          | Some pos -> pos in
        { sym with sym_offset }
      ) buf.labels
      in
      {
        seg_symbols ;
        seg_relocs = buf.relocations;
        seg_content = buf.buf;
      }
  in
  iter text_seg
