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

type symbol = {
  mutable sym_pos: int;
  sym_name: string;
  mutable value: int32;
  mutable section: [ `Num of int | `Section of section ];
  stype: int;
  storage: int;
  auxn: int;
  mutable auxs: string;
  mutable extra_info: [ `Alias of symbol | `Section of section | `None ];
}

and reloc = {
  addr: int32;
  symbol: symbol;
  rtype: int;
}

and section = {
  mutable sec_pos: int;
  sec_name: string;
  vsize: int32;
  mutable data:
    [ `String of string | `Uninit of int
    | `Lazy of in_channel * int * int ];
  mutable relocs: reloc list;
  sec_opts: int32;
}

type coff = {
  obj_name: string;
  machine: int;
  date: int32;
  mutable sections: section list;
  mutable symbols: symbol list;
  opts: int;
}

(* type machine = [< `x64 | `x86 ] *)

  module Symbol :
  sig
    val intern : section -> int32 -> symbol
    val extern : string -> symbol
    val export : string -> section -> int32 -> symbol
    val named_intern : string -> section -> int32 -> symbol
      end

  module Reloc :
  sig
    val abs :
        [< `x64 | `x86 ] -> section -> int32 -> symbol -> unit
    val rel32 :
       [< `x64 | `x86 ] -> section -> int32 -> symbol -> unit
  end

  module Section : sig
    val create : string -> int32 -> section
  end

  module Coff : sig
    val get : in_channel -> int -> int -> string -> coff
      val dump : coff -> unit
    val create : [< `x64 | `x86 ] -> coff
    val add_section : coff -> section -> unit
    val add_symbol : coff -> symbol -> unit
    val put : coff -> string
  end
