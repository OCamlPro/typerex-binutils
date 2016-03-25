
val string_of_abbrev_section : DwarfReader.at -> unit
val string_of_TAG : DwarfTypes.dwarf_TAG -> string
val string_of_lineprog_header : DwarfTypes.dwarf_CU_LN_header -> unit
val string_of_lineprg : (int * DwarfTypes.dwarf_LN_OPS) list -> unit
(*val string_of_lineprg : DwarfTypes.dwarf_LN_OPS list -> unit*)
