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


open StringCompat
open ElfTypes

let raw_format = ref false
let abstract_format = ref true
let output_file = ref None
let ocaml_format = ref false
let remove_sections = ref StringSet.empty

let arg_list = Arg.align [
    "-ocaml", Arg.Set ocaml_format, " Output in OCaml format";
    "-abstract", Arg.Set abstract_format, " Output abstract version";
    "-no-abstract", Arg.Clear abstract_format, " Don't output abstract version";
    "-raw", Arg.Set raw_format, " Output raw version";
    "-no-raw", Arg.Clear raw_format, " Don't output raw version";

    "-rm", Arg.String (fun s ->
      List.iter (fun s ->
        remove_sections := StringSet.add s !remove_sections)
        (OcpString.split s ',')
    ), "SECTIONS Comma-separated list of sections to remove";

    "-o", Arg.String (fun filename -> output_file := Some filename),
    "FILE Save file to <FILE>";
  ]

let arg_usage =
  Printf.sprintf "%s [OPTIONS] FILES" (Filename.basename Sys.argv.(0))

let _ =
  Arg.parse arg_list (fun file ->
    let raw = ElfReader.RAW.read file in

    if !raw_format then begin
      let s = if !ocaml_format then
          ElfPrinter.RAW.to_ocaml "    " raw
        else
          ElfPrinter.RAW.to_ocaml "    " raw
          (* should be to string, not yet implemented *)
      in
      Printf.printf "%s\n%!" s;
    end;

    let t = ElfReader.ABSTRACT.of_raw raw in

    if !abstract_format then begin
      let s = if !ocaml_format then
          ElfPrinter.ABSTRACT.to_ocaml "    " t
        else
          ElfPrinter.ABSTRACT.to_ocaml "    " t
          (* should be to string, not yet implemented *)
      in
      Printf.printf "%s\n%!" s;
    end;

    let t =
      if !remove_sections <> StringSet.empty then begin
        let e_sections = ref StringMap.empty in
        StringMap.iter (fun name s ->
          if not (StringSet.mem name !remove_sections) then
            e_sections := StringMap.add name s !e_sections
        ) t.ABSTRACT.e_sections;
        { t with ElfTypes.ABSTRACT.e_sections = !e_sections }
      end else t in

    match !output_file with
    | None ->
      if !remove_sections <> StringSet.empty then begin
        Printf.eprintf "Error: you must use -o together with -rm\n%!";
        exit 2
      end
    | Some filename ->
      ElfWriter.to_file filename t
  ) arg_usage

(*



let ranges = ref []
let max_name_len = ref 3

let add_range pos kind size name =
  max_name_len := max !max_name_len (String.length name);
  ranges := (pos, kind, Int64.sub Int64.zero size, name) ::
    (Int64.add pos size, -kind, size, name) ::
    !ranges

open ElfTypes

let _ =
  let ehdr = elf.elf_header in
  add_range 0L 2 ehdr.e_ehsize "elf_header";
  if ehdr.e_shnum > 0L then
    add_range ehdr.e_shoff 2 (Int64.mul ehdr.e_shnum ehdr.e_shentsize) "section table";
  if ehdr.e_phnum > 0L then
    add_range ehdr.e_phoff 2 (Int64.mul ehdr.e_phnum  ehdr.e_phentsize) "program table";

  Array.iter (fun s ->
    let shdr = s.section_header in
    if shdr.sh_size > 0L then
      add_range shdr.sh_offset 3 shdr.sh_size
        (Printf.sprintf "%s" s.section_name)
  ) elf.elf_sections;

  Array.iter (fun p ->
    let phdr = p.program_header in
    if phdr.p_filesz > 0L then
      add_range phdr.p_offset 1 phdr.p_filesz
        (Printf.sprintf "%s"
           (ElfPrinter.segment_type phdr.p_type))
  ) elf.elf_programs;

  ()

let ranges = List.sort compare !ranges



let _ =
  List.iter (fun (posL, kind, sizeL, name) ->
    Printf.printf "%8Lx   %-15s%-15s%-15s%-15s\n"
      posL
      (if kind = 1 then
          Printf.sprintf "BEGIN %s" name
       else
          if kind = -1 then
            Printf.sprintf "END %s" name
          else "")
      (if kind = 2 then
          Printf.sprintf "BEGIN %s" name
       else
          if kind = -2 then
            Printf.sprintf "END %s" name
          else "")
      (if kind = 3 then
          Printf.sprintf "BEGIN %s" name
       else
          if kind = -3 then
            Printf.sprintf "END %s" name
          else "")
      (if kind = 4 then
          Printf.sprintf "BEGIN %s" name
       else
          if kind = -4 then
            Printf.sprintf "END %s" name
          else "")
  ) ranges;
  Printf.printf "%!"

*)
