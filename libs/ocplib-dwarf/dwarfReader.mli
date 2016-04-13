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

open DwarfTypes

val read_CUs : DwarfAbbrev.abbrev_offset_table -> DwarfUtils.s -> DwarfDIE.dwarf_DIE Zipper.tree list

val read_line_prog_header : DwarfUtils.s -> DwarfLNP.dwarf_CU_LN_header
val read_line_prog_stmts : DwarfUtils.s -> DwarfLNP.dwarf_CU_LN_header -> (int * DwarfLNP.dwarf_LN_OPS) list

val read_abbrev_section : DwarfUtils.s -> DwarfAbbrev.abbrev_offset_table -> DwarfAbbrev.abbrev_offset_table
