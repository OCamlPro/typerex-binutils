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

type popular_system =
  | S_386_Linux
  | S_X64_Linux

val create :
  ElfTypes.elf_file_class ->
  ElfTypes.elf_data_encoding ->
  ElfTypes.elf_osabi -> ElfTypes.machine -> ElfTypes.ABSTRACT.t
val create_popular : popular_system -> ElfTypes.ABSTRACT.t
val to_string : ElfTypes.ABSTRACT.t -> string
val to_file : string -> ElfTypes.ABSTRACT.t -> unit
