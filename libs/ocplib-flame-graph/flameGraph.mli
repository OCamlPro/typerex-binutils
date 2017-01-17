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


type tree

type palette =
| Hot
| Mem
| IO

type rgb = int * int * int

type bts = (string list * float) list

val tree_of_bts : bts -> tree
val bts_of_tree : tree -> bts

type config = {
  mutable max_depth : int;
  mutable width : int;
  mutable palette : string -> rgb;
  mutable js : string option;
}

val new_config : unit -> config

val new_tree : string -> tree
val enter_bt : tree -> string list -> float -> unit

val set_title : tree -> string -> unit
val height_of_tree : tree -> int
val width_of_tree : tree -> float
val merge_rec : tree -> tree

val palette : palette -> string -> rgb

module type DisplayArg = sig

  type t

  val create :
    title:string ->
    ?js:string ->
    width:float ->
    height:float ->
    unit ->
    t

  val rectangle :
    t ->
    title:string ->
    caption:string ->
    x:float ->
    y:float ->
    width:float ->
    red:int ->
    green:int ->
    blue:int ->
    unit

  val to_string : t -> string

  val box_height : float

end

module type DisplayResult = sig
  val of_tree : ?config:config -> tree -> string
end

module Display(S: DisplayArg) : DisplayResult
module SVG : DisplayResult
