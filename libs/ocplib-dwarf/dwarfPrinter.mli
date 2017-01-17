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

val print_caml_locs : (int64 * DwarfLocs.location_list_entry list) list
  -> (int64, (string * string * int64 * bool)) Hashtbl.t -> unit

val print_locs : DwarfLocs.location_list_entry list list -> unit

val print_DIEs : DwarfDIE.dwarf_DIE Zipper.tree list -> DwarfUtils.s -> unit
val dump_CU_tree : string -> DwarfDIE.dwarf_DIE Zipper.tree -> unit

val print_LNPs : (DwarfLNP.dwarf_CU_LN_header * (int * DwarfLNP.dwarf_LN_OPS) list) list
  -> unit

val print_abbrevs : DwarfAbbrev.abbrev_offset_table -> unit
