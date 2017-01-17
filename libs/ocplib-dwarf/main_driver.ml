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

open ElfTypes.RAW
open Printf

let hex_flag = ref false
let oml_loc_flag = ref false
let dot_file = ref ""
let cu_num = ref (-1)
let single_section = ref ""

let arg_list = Arg.align [
    "-xxd", Arg.Set hex_flag, " output hex dump of target section";
    "-dot", Arg.Tuple ([Arg.Set_string dot_file; Arg.Set_int cu_num]), " output graph of debug_info";
    "-ml", Arg.Set oml_loc_flag, " annotate ocaml params and vars with names";
    "--section", Arg.String (fun s -> single_section := s), " target section"; ]

let arg_usage =
  sprintf "%s [OPTIONS] FILES" (Filename.basename Sys.argv.(0))

let dump_hex s =
  Xxd.output_lines s (Buffer.create 16) (Buffer.create 16) 0 (String.length s)

let is_string_empty s = (s = "")

let get_section t s =
  try
    Hashtbl.find t s
  with Not_found -> printf "error : section %s not found\n" s; exit 1

let _ =
  Arg.parse arg_list (fun file ->

      let raw = ElfReader.RAW.read file in

      let regex = (Str.regexp "debug") in
      let filter_debug_sections s_name = Str.string_match regex s_name 1 in

      let t_original = Hashtbl.create 10 in

      Array.iteri (fun i s ->
          if String.length s.section_name > 0 && filter_debug_sections s.section_name
            then Hashtbl.add t_original s.section_name s.section_content
        ) raw.elf_sections;

      if is_string_empty !single_section then
        begin
          print_endline "available sections : ";
          Hashtbl.iter (fun k v -> printf "%s\n" k) t_original
        end
      else
        begin
          let target_section = get_section t_original !single_section in

          if !hex_flag then begin
            dump_hex target_section;
            exit 1
          end;

          let target_section_stream = DwarfUtils.of_string target_section in
          let abbrev_section_stream = DwarfUtils.of_string @@ get_section t_original ".debug_abbrev" in

          (* needed to read .debug_info *)
          let abbrev_table = DwarfReader.read_abbrev_section abbrev_section_stream (Hashtbl.create 10) in

          let debug_str_section = DwarfUtils.of_string @@ get_section t_original ".debug_str" in

          let info_section_stream = DwarfUtils.of_string @@ get_section t_original ".debug_info" in
          let cus = DwarfReader.read_CUs abbrev_table info_section_stream in
          let nb_of_cus = List.length cus in

          match !single_section with
          | ".debug_info" ->
            DwarfPrinter.print_DIEs cus debug_str_section;
            if !dot_file <> "" && !cu_num <> (-1) then
              let idx = if !cu_num > (nb_of_cus-1) || !cu_num < 0 then failwith "not a valid CU index" else !cu_num in
              let ocaml_cu = List.nth cus idx in
              DwarfPrinter.dump_CU_tree !dot_file ocaml_cu

          | ".debug_line" ->
            let header_and_lnp_stmts = DwarfReader.read_header_and_lnp_stmts target_section_stream in
            DwarfPrinter.print_LNPs header_and_lnp_stmts

          | ".debug_abbrev" ->
            DwarfPrinter.print_abbrevs abbrev_table

          | ".debug_loc" ->
            (*second CU in a OCaml binary is the entry point module*)
            if !oml_loc_flag
            then
              let ocaml_cu = List.nth cus 1 in
              let locs, pv_map = DwarfReader.read_caml_locs target_section_stream ocaml_cu debug_str_section in
              DwarfPrinter.print_caml_locs locs pv_map
            else
              let locs = DwarfReader.read_all_locs target_section_stream in
              DwarfPrinter.print_locs locs

          | ".debug_frame" | ".debug_ranges" |  _ -> print_endline "other sections not supported yet"

        end;

    ) arg_usage
