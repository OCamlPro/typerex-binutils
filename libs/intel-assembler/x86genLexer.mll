{

exception SyntaxError of string * int
type token =
  | OPCODE of string
  | REG_INT of int
  | REG_REG
  | REX_W
  | PLUS
  | RB | RW | RD
  | IB | IW | ID | IO | I
  | REX

  | INS of string

  | OP_IMM8
  | OP_IMM16
  | OP_IMM32
  | OP_IMM64
  | OP_R8
  | OP_R16
  | OP_R32
  | OP_R64

  | OP_RM8
  | OP_RM16
  | OP_RM32
  | OP_RM64

  | OP_AL
  | OP_AX
  | OP_EAX
  | OP_RAX

  | OP_M16INT
  | OP_M32INT
  | OP_M64INT

  | OP_M32FP
  | OP_M64FP
  | OP_M80FP
  | OP_ST0
  | OP_STi

  | OP_XMM
  | OP_XMM1
  | OP_XMM2_M64
  | OP_XMM2_M128

  | OP_STAR

  | COMMA

  | OPS_M | OPS_O | OPS_I | OPS_R | OPS_NP

  | VALID
  | N_E
  | VV
  | SSE2
  | VNE

  | EOL
  | EOF

let string_of_token = function
  | OPCODE string -> string
  | REG_INT int -> Printf.sprintf "/%d" int
  | REG_REG -> "/r"
  | REX_W -> "REX.W"
  | PLUS -> "+"
  | RB -> "rb"
  | RW -> "rw"
  | RD -> "rd"
  | IB -> "ib"
  | IW -> "iw"
  | ID -> "id"
  | IO -> "io"
  | I -> "i"
  | REX -> "REX"

  | INS string -> string


  | OP_XMM -> "xmm"
  | OP_XMM1 -> "xmm1"
  | OP_XMM2_M64 -> "xmm2/m64"
  | OP_XMM2_M128 -> "xmm2/m128"

  | OP_IMM8 -> "imm8"
  | OP_IMM16 -> "imm16"
  | OP_IMM32 -> "imm32"
  | OP_IMM64 -> "imm64"
  | OP_R8 -> "r8"
  | OP_R16 -> "r16"
  | OP_R32 -> "r32"
  | OP_R64 -> "r64"

  | OP_RM8 -> "r/m8"
  | OP_RM16 -> "r/m16"
  | OP_RM32 -> "r/m32"
  | OP_RM64 -> "r/m64"

  | OP_AL -> "AL"
  | OP_AX -> "AX"
  | OP_EAX -> "EAX"
  | OP_RAX -> "RAX"

  | OP_M16INT -> "m16int"
  | OP_M32INT -> "m32int"
  | OP_M64INT -> "m64int"

  | OP_M32FP  -> "m32fp"
  | OP_M64FP  -> "m64fp"
  | OP_M80FP  -> "m80fp"
  | OP_ST0  -> "ST(0)"
  | OP_STi  -> "ST(i)"

  | OP_STAR -> "*"

  | COMMA -> ","

  | OPS_NP -> "NP"
  | OPS_M -> "M"
  | OPS_O -> "O"
  | OPS_I -> "I"
  | OPS_R -> "R"

  | VALID -> "Valid"
  | N_E   -> "N.E."
  | VV -> "V/V"
  | VNE -> "V/N.E."
  | SSE2 -> "SSE2"

  | EOL -> "\n"
  | EOF -> "\n\n"

let line_num = ref 0

}

let num = [ '0'-'9' ]
let spaces = [' ' '\t' '\r' '\012']+
let hexa = [ '0' - '9' 'A'-'F' ]
let letter = [ 'A'-'Z' ]
let ins = [ 'A'-'Z' ] ( letter | num )( letter | num )+

rule token = parse
  | spaces { token lexbuf }
  | '#' [^ '\n']* '\n' { incr line_num; token lexbuf }
  | '\n'    { incr line_num; EOL }
  | ':' [^ '\n']* '\n' { incr line_num; EOL }
  | "REX.W" { REX_W }
  | "REX" { REX }
  | hexa hexa { OPCODE (Lexing.lexeme lexbuf) }
  | "/r"     { REG_REG }
  | '/' num { let s = Lexing.lexeme lexbuf in
              REG_INT (int_of_char s.[1] - int_of_char '0') }
  | "+"     { PLUS }
  | "rb"   { RB }
  | "rw"   { RW }
  | "rd"   { RD }
  | "ib"   { IB }
  | "iw"   { IW }
  | "id"   { ID }
  | "io"   { IO }
  | "i"    { I }
  | ","    { COMMA }
  | "*"     { OP_STAR }
  | "N.E."  { N_E }
  | "Valid" { VALID }
  | "V/V"   { VV }
  | "V/N.E."   { VNE }
  | "SSE2"  { SSE2 }

  | "xmm"  { OP_XMM }
  | "xmm1"  { OP_XMM1 }
  | "xmm2/m64" { OP_XMM2_M64 }
  | "xmm2/m128" { OP_XMM2_M128 }

  | "imm8"    { OP_IMM8 }
  | "imm16"    { OP_IMM16 }
  | "imm32"    { OP_IMM32 }
  | "imm64"    { OP_IMM64 }

  | "r8"    { OP_R8 }
  | "r16"    { OP_R16 }
  | "r32"    { OP_R32 }
  | "r64"    { OP_R64 }

  | "r/m8"  { OP_RM8 }
  | "r/m16"  { OP_RM16 }
  | "r/m32"  { OP_RM32 }
  | "r/m64"  { OP_RM64 }

  | "m16int"  { OP_M16INT }
  | "m32int"  { OP_M32INT }
  | "m64int"  { OP_M64INT }

  | "m32fp"  { OP_M32FP }
  | "m64fp"  { OP_M64FP }
  | "m80fp"  { OP_M80FP }

  | "ST(0)"  { OP_ST0 }
  | "ST(i)"  { OP_STi }

  | "AL"     { OP_AL }
  | "AX"     { OP_AX }
  | "EAX"    { OP_EAX }
  | "RAX"    { OP_RAX }

(* all instructions are 3+ chars, except 3 ones *)
  | "BT"   { INS "BT" }
  | "IN"   { INS "IN" }
  | "OR"   { INS "OR" }
  | ins    { INS (Lexing.lexeme lexbuf) }

(* one and two-letters are for operand order *)
  | "I"    { OPS_I }
  | "R"    { OPS_R }
  | "M"    { OPS_M }
  | "O"    { OPS_O }
  | "NP"   { OPS_NP
}
  | eof    { EOF }
  | _      { raise (SyntaxError (Lexing.lexeme lexbuf, !line_num+1)) }

{
let init () =
  line_num := 0
}
