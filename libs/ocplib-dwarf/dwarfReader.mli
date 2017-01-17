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


val read_abbrev_section : DwarfUtils.s -> DwarfAbbrev.abbrev_offset_table -> DwarfAbbrev.abbrev_offset_table

val read_CUs : DwarfAbbrev.abbrev_offset_table -> DwarfUtils.s -> DwarfDIE.dwarf_DIE Zipper.tree list

val read_header_and_lnp_stmts :
  DwarfUtils.s ->
  (DwarfLNP.dwarf_CU_LN_header * (int * DwarfLNP.dwarf_LN_OPS) list) list

val read_all_locs : DwarfUtils.s -> DwarfLocs.location_list_entry list list

val read_caml_locs : DwarfUtils.s -> DwarfDIE.dwarf_DIE Zipper.tree -> DwarfUtils.s
  -> (int64 * DwarfLocs.location_list_entry list) list * (int64, (string * string * int64 * bool)) Hashtbl.t
