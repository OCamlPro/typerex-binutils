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

type offset = int

type abbrev_decl_table = (int64, dwarf_abbreviation) Hashtbl.t
type abbrev_offset_table = (offset, abbrev_decl_table) Hashtbl.t

val read_CUs : abbrev_offset_table -> DwarfUtils.s -> DwarfDIE.dwarf_DIE Zipper.tree list
val read_lineprog_section : Stream_in.s -> unit
val read_abbrev_section : Stream_in.s -> abbrev_offset_table -> abbrev_offset_table
