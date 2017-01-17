(**************************************************************************)
(*                                                                        *)
(*                        OCamlPro Typerex                                *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the LGPL v3.0            *)
(*   (GNU Lesser General Public Licence version 3.0).                     *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
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
