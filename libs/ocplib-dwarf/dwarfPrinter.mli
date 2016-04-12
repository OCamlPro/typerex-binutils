val string_of_abbrev_section : DwarfReader.abbrev_decl_table -> unit
val string_of_TAG : DwarfTypes.dwarf_TAG -> string
val string_of_lineprog_header : DwarfTypes.dwarf_CU_LN_header -> unit
val string_of_lineprg : (int * DwarfTypes.dwarf_LN_OPS) list -> unit

val string_of_abbrev_decl : DwarfTypes.dwarf_abbreviation -> unit
val string_of_DIE : DwarfDIE.dwarf_DIE -> unit
