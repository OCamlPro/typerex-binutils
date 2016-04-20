val string_of_abbrev_section : DwarfAbbrev.abbrev_decl_table -> unit
val string_of_TAG : DwarfTypes.dwarf_TAG -> string
val string_of_lineprog_header : DwarfLNP.dwarf_CU_LN_header -> unit
val string_of_lineprg : (int * DwarfLNP.dwarf_LN_OPS) list -> unit

val string_of_abbrev_decl : DwarfAbbrev.dwarf_abbreviation -> unit
val string_of_DIE : DwarfDIE.dwarf_DIE -> DwarfUtils.s -> unit
