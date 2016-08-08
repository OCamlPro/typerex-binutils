
val new_segment: unit -> IntelasmTypes.segment
val clear_segment: IntelasmTypes.segment -> unit

val def_label: IntelasmTypes.segment -> int -> unit
val label_name: int -> string
val symbol_name: string -> string

val emit:
  IntelasmTypes.segment ->
  IntelasmTypes.instr -> IntelasmTypes.arg list -> unit

val emit_int: int -> IntelasmTypes.arg
val emit_int32: int32 -> IntelasmTypes.arg
val emit_int64: int64 -> IntelasmTypes.arg
val emit_nativeint: nativeint -> IntelasmTypes.arg

val emit_label_rel: int -> IntelasmTypes.arg
val emit_label_rip: int -> IntelasmTypes.arg
val emit_label_abs: int -> IntelasmTypes.arg

val emit_global_label: IntelasmTypes.segment -> string -> unit

val emit_symbol_rel: string -> IntelasmTypes.arg
val emit_symbol_rip: string -> IntelasmTypes.arg
val emit_symbol_abs: string -> IntelasmTypes.arg

