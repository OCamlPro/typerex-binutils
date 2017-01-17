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

val new_segment: unit -> IntelasmTypes.segment
val clear_segment: IntelasmTypes.segment -> unit

val def_label: IntelasmTypes.segment -> int -> unit
val label_name: int -> string
val symbol_name: string -> string

val emit:
  IntelasmTypes.segment ->
  IntelasmTypes.instr -> IntelasmTypes.arg list -> unit

val emit_int: int -> IntelasmTypes.arg
val emit_int32: int32 -> IntelasmTypes.arg
val emit_int64: int64 -> IntelasmTypes.arg
val emit_nativeint: nativeint -> IntelasmTypes.arg

val emit_label_rel: int -> IntelasmTypes.arg
val emit_label_rip: int -> IntelasmTypes.arg
val emit_label_abs: int -> IntelasmTypes.arg

val emit_global_label: IntelasmTypes.segment -> string -> unit

val emit_symbol_rel: string -> IntelasmTypes.arg
val emit_symbol_rip: string -> IntelasmTypes.arg
val emit_symbol_abs: string -> IntelasmTypes.arg

