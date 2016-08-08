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

type indent = string

module RAW : sig
  val to_ocaml : indent -> ElfTypes.RAW.t -> string
end


module ABSTRACT : sig
  val to_ocaml : indent -> ElfTypes.ABSTRACT.t -> string
end

val string_of_data_encoding : ElfTypes.elf_data_encoding -> string
