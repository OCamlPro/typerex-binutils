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
(***********************************************************************)
(*                                                                     *)
(*           Written by Fabrice LE FESSANT, OcamlPro                  *)
(*                                                                     *)
(*                   for Lexify SAS                                    *)
(*                     (2009/12)                                       *)
(***********************************************************************)

open StringCompat

open BinobjFile

type condition =
  | O
  | NO
  | B | C | NAE
  | NB | NC | AE
  | Z | E
  | NZ | NE
  | BE | NA
  | NBE | A
  | S
  | NS
  | P | PE
  | NP | PO
  | L | NGE
  | NL | GE
  | LE | NG
  | NLE | G

type locality =
    Loc_unknown
  | Loc_near (* 8 bits offset *)
  | Loc_far  (* 32 bits offset *)

type rounding =
  | RoundUp
  | RoundDown
  | RoundNearest
  | RoundTruncate

type instr =
    Global of string
  | Byte
  | Word
  | Dword
  | Qword
  | Align of bool * int
  | NewLabel of string
  | Bytes of string
  | Comment of string

  | ADD
  | SUB
  | XOR
  | OR
  | AND
  | CMP

  | FSTPL

  | SARQ
  | SHRQ
  | SALQ

  | INCQ
  | DECQ

  | IMULQ
  | IDIVQ

  | MOVB
  | MOVW
  | MOVL
  | MOV
  | MOVZX_byte (* MOVZBQ *)
  | MOVSX_byte (* MOVSBQ *)
  | MOVSS
  | MOVZX_word (* MOVZWQ *)
  | MOVSX_word (* MOVSWQ *)
  | MOVSXD (* MOVSLQ *)

  | MOVSD
  | ADDSD
  | SUBSD
  | MULSD
  | DIVSD
  | SQRTSD
  | ROUNDSD of rounding
  | NEG

  | CVTSS2SD
  | CVTSD2SS
  | CVTSI2SD
  | CVTSD2SI
  | CVTTSD2SI
  | UCOMISD
  | COMISD

  | CALL
  | JMP of locality
  | RET
  | PUSH
  | POP

  | TESTQ
  | SET of condition
  | J of locality * condition

  | CMOV of condition
  | XORPD
  | ANDPD
  | MOVAPD
  | MOVLPD

  | LEAQ
  | CQTO
  | LEAVE

  | XCHG
  | BSWAP

type register64 =
  | RAX | RBX | RDI | RSI | RDX | RCX | RBP | RSP
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  | RIP

type register8 =
    AL | BL | CL | DL
  | AH | BH | CH | DH
  | DIL | SIL | R8B | R9B |
   R10B | R11B | BPL | R12B | R13B | SPL | R14B | R15B

type register16 =
    AX | BX | DI | SI | DX | CX | SP | BP
  | R8W | R9W | R10W | R11W | R12W | R13W | R14W | R15W

type register32 =
    EAX | EBX | EDI | ESI | EDX | ECX | R8D | R9D |
   R10D | R11D | EBP | R12D | R13D | R14D | R15D | ESP

type registerf = XMM of int

type 'reg base =
    NoBase
  | BaseReg of 'reg
  | BaseSymbol of string

type arg =
  | Constant64 of int64
  | LabelRel of string * int64
  | LabelAbs of string * int64
  | Offset of string (* for non pic-code ? *)

  | Reg8 of register8
  | Reg16 of register16
  | Reg32 of register32
  | Reg of register64
  | Regf of registerf
  | Mem of register64 * (* scale *) int * register64 base * (* offset *) int


type instruction = {
  mutable pos : int;
  mutable instr : instr;
  mutable args : arg list;
}

type segment = {
  mutable seg_labels : int StringMap.t;
  mutable seg_pos : int;
  mutable seg_instrs : instruction Queue.t;
}
