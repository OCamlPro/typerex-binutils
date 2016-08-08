open Intel_proc

type buffer

val rex : int
val no_rex : int
val rexw : int


val rd_of_reg8 : register8 -> int
val rd_of_reg16 : register16 -> int
val rd_of_reg32 : register32 -> int
val rd_of_reg64 : register64 -> int
val rd_of_regf : registerf -> int
val rex_opcode : int -> int
val reg7 : int -> int

val buf_int32_imm : buffer -> Intel_proc.offset -> unit
val buf_int64_imm : buffer -> Intel_proc.offset -> unit
val buf_int8 : buffer -> int -> unit
val buf_int8L : buffer -> int64 -> unit
val buf_int16L : buffer -> int64 -> unit
val buf_int32L : buffer -> int64 -> unit
val buf_int64L : buffer -> int64 -> unit
val buf_opcodes : buffer -> int list -> unit
val emit_mod_rm_reg : buffer -> int -> int list -> Intel_proc.arg -> int -> unit
val emit_rex : buffer -> int -> unit
