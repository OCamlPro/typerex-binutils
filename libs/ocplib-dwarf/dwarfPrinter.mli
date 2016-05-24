val print_caml_locs : DwarfLocs.location_list_entry list list
                 -> (int64, (string * string * int64 * bool)) Hashtbl.t -> unit

val print_locs : DwarfLocs.location_list_entry list list -> unit

val print_DIEs : DwarfDIE.dwarf_DIE Zipper.tree list -> DwarfUtils.s -> unit
val dump_CU_tree : string -> DwarfDIE.dwarf_DIE Zipper.tree -> unit

val print_LNPs : (DwarfLNP.dwarf_CU_LN_header * (int * DwarfLNP.dwarf_LN_OPS) list) list
                 -> unit

val print_abbrevs : DwarfAbbrev.abbrev_offset_table -> unit
