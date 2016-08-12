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

val read_abbrev_section : DwarfUtils.s -> DwarfAbbrev.abbrev_offset_table -> DwarfAbbrev.abbrev_offset_table

val read_CUs : DwarfAbbrev.abbrev_offset_table -> DwarfUtils.s -> DwarfDIE.dwarf_DIE Zipper.tree list

val read_header_and_lnp_stmts :
  DwarfUtils.s ->
  (DwarfLNP.dwarf_CU_LN_header * (int * DwarfLNP.dwarf_LN_OPS) list) list

val read_all_locs : DwarfUtils.s -> DwarfLocs.location_list_entry list list

val read_caml_locs : DwarfUtils.s -> DwarfDIE.dwarf_DIE Zipper.tree -> DwarfUtils.s
  -> (int64 * DwarfLocs.location_list_entry list) list * (int64, (string * string * int64 * bool)) Hashtbl.t
