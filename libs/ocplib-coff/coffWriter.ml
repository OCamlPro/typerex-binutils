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
open BinobjFile

module COFF = struct

  open Coff

  let create_coff proc text data =
    let coff = Coff.create proc.proc_machine in

    let align = match proc.proc_machine with
        `x64 -> 0x0050_0000_l (* IMAGE_SCN_ALIGN_16BYTES *)
      | `x86 -> 0x0030_0000_l (* IMAGE_SCN_ALIGN_4BYTES *)
    in
    let sects = [
        (* IMAGE_SCN_MEM_WRITE + IMAGE_SCN_MEM_READ + IMAGE_SCN_ALIGN_4BYTES + 401 *)
        (* Why 401 ? *)
      Section.create ".data"
        (Int32.logor align 0xC000_0040l), data;
        (* IMAGE_SCN_MEM_READ + IMAGE_SCN_MEM_EXECUTE + IMAGE_SCN_ALIGN_4BYTES + IMAGE_SCN_LNK_INFO + 01 *)
        (* Why 01 ? *)
      Section.create ".text"
        (Int32.logor align 0x6000_0020l), text;
    ]
    in
    let syms = Hashtbl.create 16 in
    let interns = Hashtbl.create 16 in
    let create_section (sect, seg) =
      Coff.add_section coff sect;

        (*
          StringSet.iter (fun s ->
          Printf.fprintf stderr "GLOBAL [%s]\n%!" s
          ) b.globals;

          StringMap.iter (fun s pos ->
          Printf.fprintf stderr "LABEL [%s] -> %d\n%!" s pos
          ) b.labels;
        *)

      StringMap.iter
        (fun s sym ->
          let pos = sym.sym_offset in
          let coff_sym =
            if sym.sym_global
            then begin
                 (*          Printf.fprintf stderr "global %s\n%!" s; *)
              Symbol.export s sect (Int32.of_int pos)
            end
            else begin
                 (*          Printf.fprintf stderr "intern %s\n%!" s; *)
              Hashtbl.replace interns s false;
              Symbol.named_intern s sect (Int32.of_int pos)
            end
          in
          Hashtbl.replace syms s coff_sym;
          Coff.add_symbol coff coff_sym;
        ) seg.seg_symbols
    in
    List.iter create_section sects;
    List.iter
      (fun (sect, b) ->

        let add_local_symbol reloc s pos =
               (*         Printf.fprintf stderr "add_local_symbol %s\n%!" s; *)
          if Hashtbl.mem interns s then
            Hashtbl.replace interns s true;

          let sym =
            try Hashtbl.find syms s
            with Not_found ->
              let sym = Symbol.extern s in
              Hashtbl.replace syms s sym;
              Coff.add_symbol coff sym;
              sym
          in
          reloc proc.proc_machine sect (Int32.of_int pos) sym
        in

        BinobjFile.relocate_segment b
          (add_local_symbol Reloc.rel32) (add_local_symbol Reloc.abs);
        sect.data <- `String (Bytes.to_string b.seg_content);
      ) sects;

    coff.symbols <-
      (List.filter
         (fun sym -> try Hashtbl.find interns sym.sym_name with Not_found -> true)
         coff.symbols
      );
    coff

  let put coff = Coff.put coff

end (* end of COFF module *)


let to_string t =
  let coff = COFF.create_coff t.bin_proc t.bin_text t.bin_data in
  COFF.put coff
