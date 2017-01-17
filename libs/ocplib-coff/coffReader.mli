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

module Library : sig

  exception FileIsNotALibrary of string
  exception InvalidLibraryFormat of string

  type member_kind =
      MberLinker of bool
    | MberStringTable of string
    | MberObject of string

  type member = {
    mber_pos : int;

    mber_name : string;
    mber_date : string;
    mber_user : string;
    mber_group : string;
    mber_mode : string;
    mber_size : int;
    mber_kind : member_kind;
  }

  type library = {
    lib_name : string;
    lib_ic : in_channel;
    lib_members : member array;
  }

  val read: string -> library

  val string_of_kind: member_kind -> string
  val read_linker1: library -> (string * member) array
  val read_linker2: library -> (string * member) array option
  val iter_objects:
    (library -> int -> CoffTypes.RAW.object_file -> unit) ->
    library -> unit
end

module PEFile : sig
  val stub : string
  val read : string -> in_channel CoffTypes.RAW.pe_file
end

module Object : sig
  val read: string -> CoffTypes.RAW.object_file
end
