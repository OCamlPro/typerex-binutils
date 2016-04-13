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

open DwarfUtils
open DwarfTypes
open DwarfPrinter
open DwarfFormat

let read_abbrev_section s t = DwarfAbbrev.read_abbrev_section s t

let read_CUs abbrev_tbl s = DwarfDIE.readAllDIE abbrev_tbl s

let read_line_prog_header s = DwarfLNP.read_line_prog_header s

let read_line_prog_stmts s h = DwarfLNP.read_line_prog_stmts s h
