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

open BinobjFile
open IntelasmTypes
open IntelasmEmit

let tab b = Buffer.add_char b '\t'
let print b s = tab b; Buffer.add_string b s

let string_of_register64 proc reg64 =
  if proc.proc_arch64 then
    match reg64 with
    | RAX -> "rax"
    | RBX -> "rbx"
    | RDI -> "rdi"
    | RSI -> "rsi"
    | RDX -> "rdx"
    | RCX -> "rcx"
    | RBP -> "rbp"
    | RSP -> "rsp"
    | R8 -> "r8"
    | R9 -> "r9"
    | R10 -> "r10"
    | R11 -> "r11"
    | R12 -> "r12"
    | R13 -> "r13"
    | R14 -> "r14"
    | R15 -> "r15"
    | RIP -> "rip"
  else
    match reg64 with
    | RAX -> "eax"
    | RBX -> "ebx"
    | RDI -> "edi"
    | RSI -> "esi"
    | RDX -> "edx"
    | RCX -> "ecx"
    | RBP -> "ebp"
    | RSP -> "esp"
    | RIP -> "sip"
    | _ -> assert false

let string_of_register8 reg8 = match reg8 with
  | AL -> "al"
  | BL -> "bl"
  | DL -> "dl"
  | CL -> "cl"
  | AH -> "ah"
  | BH -> "bh"
  | CH -> "ch"
  | DH -> "dh"
  | DIL -> "dil"
  | SIL -> "sil"
  | R8B -> "r8b"
  | R9B -> "r9b"
  | R10B -> "r10b"
  | R11B -> "r11b"
  | BPL -> "bpl"
  | R12B -> "r12b"
  | R13B -> "r13b"
  | SPL -> "spl"
  | R14B -> "r14b"
  | R15B -> "r15b"

let string_of_register16 reg16 =
  match reg16 with
    AX -> "ax"
  | BX -> "bx"
  | DI -> "di"
  | SI -> "si"
  | DX -> "dx"
  | CX -> "cx"
  | SP -> "sp"
  | BP -> "bp"
  | R8W -> "r8w"
  | R9W -> "r9w"
  | R10W -> "r10w"
  | R11W -> "r11w"
  | R12W -> "r12w"
  | R13W -> "r13w"
  | R14W -> "r14w"
  | R15W -> "r15w"

let string_of_register32 reg32 = match reg32 with
    EAX -> "eax"
  | EBX -> "ebx"
  | EDI -> "edi"
  | ESI -> "esi"
  | EDX -> "edx"
  | ECX -> "ecx"
  | ESP -> "esp"
  | EBP -> "ebp"
  | R8D -> "r8d"
  | R9D -> "r9d"
  | R10D -> "r10d"
  | R11D -> "r11d"
  | R12D -> "r12d"
  | R13D -> "r13d"
  | R14D -> "r14d"
  | R15D -> "r15d"

let string_of_registerf regf = match regf with
  | XMM n -> Printf.sprintf "xmm%d" n

let string_of_condition condition = match condition with
    E -> "e"
  | AE -> "ae"
  | A -> "a"
  | GE -> "ge"
  | G -> "g"
  | NE -> "ne"
  | B -> "b"
  | BE -> "be"
  | L -> "l"
  | LE -> "le"
  | NLE -> "nle"
  | NG -> "ng"
  | NL -> "nl"
  | NGE -> "nge"
  | PO -> "po"
  | NP -> "np"
  | PE -> "pe"
  | P -> "p"
  | NS -> "ns"
  | S -> "s"
  | NBE -> "nbe"
  | NA -> "na"
  | NZ -> "nz"
  | Z -> "z"
  | NC -> "nc"
  | NB -> "nb"
  | NAE -> "nae"
  | C -> "c"
  | NO -> "no"
  | O -> "o"

let print_arg b proc ins arg =
  match arg with
      (*      | Constant int ->
              Printf.bprintf b "$0x%x" int
              | Constant32 int32 ->
              Printf.bprintf b "$0x%lx" int32 *)
  | Constant64 int64 ->
    Printf.bprintf b "$0x%Lx" int64
  | LabelRel (string, iL) ->
    Printf.bprintf b "REL32 $%s+%Ld" string iL
  | LabelAbs (string, iL) ->
    Printf.bprintf b "DIR64 $%s+%Ld" string iL
  | Offset string (* for non pic-code ? *) ->
    assert false

  | Reg8 register8 ->
    Printf.bprintf b "%%%s" (string_of_register8 register8)
  | Reg16 register16 ->
    Printf.bprintf b "%%%s" (string_of_register16 register16)
  | Reg32 register32 ->
    Printf.bprintf b "%%%s" (string_of_register32 register32)
  | Reg register64 ->
    Printf.bprintf b "%%%s" (string_of_register64 proc register64)
  | Regf registerf ->
    Printf.bprintf b "%%%s" (string_of_registerf registerf)

  | Mem (reg1, 1, NoBase, offset) ->
    begin
      if offset < 0 then
        Printf.bprintf b "-0x%x" (-offset)
      else
        Printf.bprintf b "0x%x" offset
    end;
    Buffer.add_char b '(';
    Printf.bprintf b "%%%s" (string_of_register64 proc reg1);
    Buffer.add_char b ')'

  | Mem (reg1, scale, reg2, offset) ->
    begin
      if offset < 0 then
        Printf.bprintf b "-0x%x" (-offset)
      else
        if offset = 0 && ins.instr = MOVZX_byte then () else
          Printf.bprintf b "0x%x" offset
    end;
    Buffer.add_char b '(';
    begin
      match reg2 with
        NoBase -> ()
      | BaseReg reg2 ->
        Printf.bprintf b "%%%s" (string_of_register64 proc reg2)
      | BaseSymbol s ->
        Printf.bprintf b "%s" s
    end;
    Buffer.add_char b ',';
    Printf.bprintf b "%%%s" (string_of_register64 proc reg1);
    Printf.bprintf b ",%d" scale;
    Buffer.add_char b ')'

let print_args b proc instr =
  match instr with
    { args = [] } -> ()
  | { args = [Reg _ as arg]; instr = (CALL | JMP _) } ->
    tab b; Buffer.add_char b '*'; print_arg b proc instr arg
  | { args = [arg] } -> tab b; print_arg b proc instr arg
  | { args = [arg2; arg1] } ->
    tab b; print_arg b proc instr arg1; Buffer.add_char b ','; print_arg b proc instr arg2
  | _ -> assert false

let print_instr b proc instr =
  begin
    match instr.instr with
      Global s ->
        Printf.bprintf b ".global %s" s;
    | Align (data,n) ->
      Printf.bprintf b ".align %d" n
    | NewLabel s ->
      Printf.bprintf b "%s:" s
    | Comment s ->
      Printf.bprintf b "\t\t\t\t(* %s *)" s
    | _ ->
      print b (match instr.instr with
        Global _ | Align _ | NewLabel _ | Comment _ -> assert false

      | Byte ->   "byte"
      | Word ->  "word"
      | Dword ->  "dword"
      | Qword ->  "qword"
      | Bytes s ->
        Printf.sprintf "bytes \"%s\"" (String.escaped s)

      | NEG ->  "neg"
      | ADD ->  "add"
      | SUB ->  "sub"
      | XOR ->  "xor"
      | OR ->  "or"
      | AND ->  "and"
      | CMP ->  "cmp"

      | LEAVE -> "leave"
      | SARQ ->  "sar"
      | SHRQ ->  "shr"
      | SALQ ->  "shl"

      | FSTPL -> "fstpl"

      | INCQ ->  "inc"
      | DECQ ->  "dec"

      | IMULQ ->  "imulq"
      | IDIVQ ->  "idivq"

      | MOVB ->  "movb"
      | MOVW ->  "movw"
      | MOV ->
        (match instr.args with
               (*          | [_; Constant64 iL] when iL > 127L || iL < -128L   -> "movq"
                           | [_; Constant64 iL] when iL > 127L || iL < -128L   -> "movq" *)
        | _ -> "mov")
      | MOVZX_byte ->  "movzbq"
      | MOVSX_byte ->  "movsbq"
      | MOVSS ->  "movss"
      | MOVZX_word ->  "movzwq"
      | MOVSX_word ->  "movswq"
      | MOVSXD ->  "movslq"

      | MOVSD ->  "movsd"
      | ADDSD ->  "addsd"
      | SUBSD ->  "subsd"
      | MULSD ->  "mulsd"
      | DIVSD ->  "divsd"
      | SQRTSD -> "sqrtsd"
      | ROUNDSD rounding ->
        Printf.sprintf "roundsd.%s" (match rounding with
          RoundDown -> "down"
        | RoundUp -> "up"
        | RoundTruncate -> "trunc"
        | RoundNearest -> "near")
      | CVTSS2SD ->  "cvtss2sd"
      | CVTSD2SS ->  "cvtsd2ss"
      | CVTSI2SD ->  "cvtsi2sd"
      | CVTSD2SI ->  "cvtsd2si"
      | CVTTSD2SI ->  "cvttsd2si"
      | UCOMISD ->  "ucomisd"
      | COMISD ->  "comisd"

      | CALL  ->  "callq"
      | JMP _ ->  "jmpq"
      | RET ->  "retq"
      | PUSH ->  "push"
      | POP ->  "pop"

      | TESTQ ->  "testq"
      | SET condition ->
        Printf.sprintf  "set%s" (string_of_condition condition)
      | J (_,condition) ->
        Printf.sprintf  "j%s" (string_of_condition condition)

      | CMOV condition ->
        Printf.sprintf "cmov%s" (string_of_condition condition)
      | XORPD ->  "xorpd"
      | ANDPD ->  "andpd"
      | MOVLPD ->  "movlpd"
      | MOVAPD ->  "movapd"

      | LEAQ ->  "lea"
      | CQTO ->  "cqto"

      | MOVL ->  "movl"
      | BSWAP -> "bswap"
      | XCHG -> "xchg"
      )
  end;
  print_args b proc instr;
  Buffer.add_string b "\n"

let string_of_instr proc ins =
  let b = Buffer.create 100 in
  print_instr b proc ins;
  Buffer.contents b

let string_of_segment proc seg =
  let b = Buffer.create 10000 in
  Queue.iter (fun instr ->
    print_instr b proc instr)
    seg.seg_instrs;
  Buffer.contents b
