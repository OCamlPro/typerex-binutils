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

let read_abbrev_section = DwarfAbbrev.read_abbrev_section

let read_CUs = DwarfDIE.readAllDIE

let read_header_and_lnp_stmts = DwarfLNP.readLNPs

let read_locs = DwarfLocs.read_locs_of_CU

let read_caml_locs = DwarfLocs.read_caml_locs
