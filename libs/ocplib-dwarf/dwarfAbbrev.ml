open DwarfTypes
open DwarfUtils

type offset = int

type dwarf_abbreviation =
  { abbrev_num : Word64.t;
    abbrev_tag : dwarf_TAG;
    abbrev_has_children : bool;
    abbrev_attributes : (dwarf_AT * Form.dwarf_FORM) list; }

(* Maps an abbreviation number to a dwarf_abbreviation record. *)
type abbrev_decl_table = (int64, dwarf_abbreviation) Hashtbl.t

(* Maps a .debug_abbrev section offset to a set of abbreviation records. *)
(* That offset is found in a CU header and thus associate that CU with the *)
(* DIE abbreviations. *)
type abbrev_offset_table = (offset, abbrev_decl_table) Hashtbl.t

let (=*=) a b =
  match Int64.compare a b with
  | 1 | -1 -> false
  | _ -> true

let z64 = Int64.zero

let both_int64_are_zero a b = a =*= z64 && b =*= z64

let read_abbrev_declaration s decl_code =
  let tag = dw_tag (Int64.to_int (read_uleb128 s)) in
  let has_children = if read_int8 s = 0x01 then true else false in

  let rec attr_helper s =
    let attr_name = read_uleb128 s in
    let attr_form = read_uleb128 s in
    if both_int64_are_zero attr_name attr_form
    then []
    else (dw_at (Int64.to_int attr_name),
          Form.dw_form (Int64.to_int attr_form)) :: attr_helper s in

  { abbrev_num = decl_code;
    abbrev_tag = tag;
    abbrev_has_children = has_children;
    abbrev_attributes = attr_helper s; }

(* One issue with keeping abbreviations in a hash table is that if one wants to
 * print them e.g for debugging purposes, they will not be sorted. *)
let read_abbrev_section s t =
  let offset_tbl : abbrev_decl_table ref = ref (Hashtbl.create 10) in
  let exit = ref true in
  let curr_offset = ref 0 in
  while !exit do
    let decl_code = read_uleb128 s in
    begin
      (*The abbreviations for a given compilation unit end with *)
      (*an entry with an abbreviation code of 0.*)
      if decl_code =*= z64
      then begin
        Hashtbl.add t !curr_offset !offset_tbl;
        curr_offset := !(s.offset);
        offset_tbl := Hashtbl.create 10
      end
      else begin
        let abbrev_declaration = read_abbrev_declaration s decl_code in
        Hashtbl.add !offset_tbl decl_code abbrev_declaration;
      end
    end;

    if DwarfUtils.peek s = None then exit := false;
  done;
  t
