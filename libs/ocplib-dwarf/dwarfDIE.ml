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

open Zipper
open DwarfUtils
open DwarfTypes
open DwarfFormat
open DwarfAbbrev

type dwarf_DIE_header =
  { format: dwarf_format;
    unit_length : int64;
    abbrev_offset : int64;
    address_size : int;
    version : int;
    initial_length_size : int;
    abbrev_offset_size : int; }

type dwarf_DIE =
  { die_ofs : int;
    die_cu_header : dwarf_DIE_header option;
    has_children: bool;
    depth:    int;
    abbrev_nu    : int64;
    die_tag          : dwarf_TAG;
    die_attributes   : (dwarf_AT * Form.dwarf_FORM) list;
    die_attribute_vals: (int * (Class.form_class * Form_data.form_data)) list;}

let get_abbrev_offset s dwf =
  match dwf with
  | DWF_32BITS -> Int64.of_int32 @@ read_int32 s
  | DWF_64BITS -> read_int64 s

let empty_DIE =
  { die_ofs = 0;
    die_cu_header = None;
    has_children = false;
    depth = 0;
    abbrev_nu = Int64.zero;
    die_tag = DW_TAG_user Int64.zero;
    die_attributes = [];
    die_attribute_vals = []; }

(*As binary data, .debug_info CU DIEs are flattened k-ary trees.*)
let deserialize input =
  let rec helper z input =
    let (cnode, p) = z in
    match input with
    | Some (x) :: rest ->
      let nn = Branch(x, []) in
      let upd_z = insert_down z nn in
      if x.has_children
      then
        let res = match cnode with
          (*no childs? we move down one level deeper*)
          | Branch(_, []) -> move_down upd_z
          (* guaranteed to have at least 1 child *)
          | Branch(_,_::_) -> last_child_of_pos (move_down upd_z)
        in helper res rest
      else helper upd_z rest
    | [None] -> z
    | [] -> z
    | None :: rest -> helper (move_up z) rest in

  match input with
  | [] -> failwith "no input"
  | None::tl -> failwith "invalid"
  | Some(hd)::tl ->
    let (final_tree, path) = helper (leaf hd, Top) tl in
    match path with Top -> final_tree | _ -> failwith "problem with tree building"

let read_DIE_header s =
  let (dwarf_format, initial_length) = DwarfUtils.get_initial_length s in
  let version = read_int16 s in
  let abbrev_offset = get_abbrev_offset s dwarf_format in
  let address_size = read_int8 s in

  let initial_length_size = match dwarf_format with
      DWF_32BITS -> 4
    | DWF_64BITS -> 12 in

  let abbrev_offset_size = match dwarf_format with
      DWF_32BITS -> 4
    | DWF_64BITS -> 8 in

  { format = dwarf_format;
    unit_length = initial_length;
    abbrev_offset = abbrev_offset;
    address_size = address_size;
    version = version;
    initial_length_size = initial_length_size;
    abbrev_offset_size = abbrev_offset_size; }

let get_abbrev_decl tbl code =
  try Some(Hashtbl.find tbl code) with Not_found -> None

let read_DIE_attrs d s =
  List.map (fun (n,f) -> Form.get_form s f) d.DwarfAbbrev.abbrev_attributes

let readADIE abtbl s h cuofs =

  let lvl = ref 0 in
  let res = ref [] in
  let exit = ref true in

  let is_cu = ref true in
  let abbrev_code_ofs = ref 0 in

  while !exit do
    abbrev_code_ofs := !(s.offset);
    let die_abbrev_code = read_uleb128 s in

    (*Did we reach a null DIE entry?*)
    (*Check the depth and stop if we already reached the root.*)
    match get_abbrev_decl abtbl die_abbrev_code with
    | None ->
      (res := !res @ [None];
       if !lvl > 1 then
         lvl := !lvl - 1
       else
         exit := false)
    | Some(d) ->
      let vals = read_DIE_attrs d s in
      let cu =
        match !is_cu with
        | false ->
          {empty_DIE with
           die_ofs = !abbrev_code_ofs;
           depth = !lvl;
           die_attribute_vals = vals;
           die_tag = d.abbrev_tag;
           abbrev_nu = die_abbrev_code;
           die_attributes = d.abbrev_attributes}
        | true ->
          is_cu := false;
          {empty_DIE with
           die_ofs = cuofs;
           depth = !lvl;
           die_cu_header = h;
           die_attribute_vals = vals;
           die_tag = d.abbrev_tag;
           abbrev_nu = die_abbrev_code;
           die_attributes = d.abbrev_attributes} in
      res := !res @ [Some({cu with has_children = d.abbrev_has_children})];

      match d.abbrev_has_children, !lvl with
      (*CU DIE root level, no more children DIEs to process*)
      | false, 0 -> exit := false
      (*There are still some DIEs left to process*)
      | true, l -> lvl := !lvl + 1
      (*A null DIE is close, handle this at start of next iteration*)
      | false, l -> ()
  done;
  !res

let readAllDIE abbrev_tbl s =

  let get_abbrev_tbl ofs =
    try
      Some(Hashtbl.find abbrev_tbl ofs)
    with Not_found -> None in

  let res = ref [] in

  while DwarfUtils.peek s != None do
    let cu_offset = ref !(s.offset) in
    let dw_DIE_CU_header = read_DIE_header s in
    let abbrev_offset = dw_DIE_CU_header.abbrev_offset in

    (match get_abbrev_tbl (Int64.to_int abbrev_offset) with
     (*retrieving set of abbreviations for current CU DIE*)
     | Some(curr_offset_tbl) ->
       let cu_dies = readADIE curr_offset_tbl s (Some(dw_DIE_CU_header)) !cu_offset in
       let cuu = deserialize cu_dies in
       res := !res @ [cuu]
     | None -> ());
    cu_offset := !(s.offset)
  done;
  !res
