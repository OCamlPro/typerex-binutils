open ElfTypes.RAW
open Printf
open Zipper
open DwarfDIE

let hex_flag = ref false
let dot_file = ref ""
let single_section = ref ""

let arg_list = Arg.align [
    "-xxd", Arg.Set hex_flag, " output hex dump of target section";
    "-dot", Arg.String (fun s -> dot_file := s), " output graph of debug_info";
    "--section", Arg.String (fun s -> single_section := s), " target section";
  ]

let arg_usage =
  Printf.sprintf "%s [OPTIONS] FILES" (Filename.basename Sys.argv.(0))

let dump_hex s =
  Xxd.output_lines s (Buffer.create 16) (Buffer.create 16) 0 (String.length s)

let is_string_empty s = (s = "")

let get_section t s =
    try
        Hashtbl.find t s
    with Not_found -> Printf.printf "error : section %s not found\n" s; exit 1

let _ =
  Arg.parse arg_list (fun file ->

    let raw = ElfReader.RAW.read file in

    let regex = (Str.regexp "debug") in
    let filter_debug_sections s_name = Str.string_match regex s_name 1 in

    let t_original = Hashtbl.create 10 in

    Array.iteri (fun i s ->
        if String.length s.section_name > 0 then begin
            if filter_debug_sections s.section_name
            then begin
                Hashtbl.add t_original s.section_name s.section_content;
            end
        end;
    ) raw.elf_sections;

    if is_string_empty !single_section then
        begin
        print_endline "available sections : ";
        Hashtbl.iter (fun k v -> Printf.printf "%s\n" k) t_original
        end
    else
        begin
        let target_section = get_section t_original !single_section in
        let section_stream = DwarfUtils.of_string target_section in

        if !hex_flag then begin
            dump_hex target_section;
            exit 1
        end;

        match !single_section with
        | ".debug_info" ->
                begin
                    let abbrev_section_stream = DwarfUtils.of_string @@ get_section t_original ".debug_abbrev" in
                    let ds = DwarfUtils.of_string @@ get_section t_original ".debug_str" in
                    let abbrev_table = DwarfReader.read_abbrev_section abbrev_section_stream (Hashtbl.create 10) in
                    let sec = DwarfUtils.of_string @@ target_section in
                    let cus = DwarfReader.read_CUs abbrev_table sec in
                    print_endline "Contents of the .debug_info section:";
                    print_endline "";
                    List.iter (fun t ->
                        Zipper.fold_tree2 (fun x -> DwarfPrinter.string_of_DIE x ds) (fun x ys -> ()) t
                    ) cus;

                    if !dot_file <> "" then begin

                        let rec trav t = match t with
                                        | Branch(x, []) -> [sprintf "    h_0x%x;\n" x.die_ofs]
                                        | Branch(x, cs) -> List.map (fun c ->
                                                match c with Branch(cc,_) ->
                                                sprintf "    h_0x%x -> h_0x%x;\n" x.die_ofs cc.die_ofs) cs @
                                                 List.concat @@ List.map trav cs in

                        let oc = open_out (!dot_file ^ ".dot") in
                        fprintf oc "digraph BST {\n";
                        fprintf oc "    nodesep=0.4;\n";
                        fprintf oc "    ranksep=0.5;\n";
                        fprintf oc "    node [fontname=\"Arial\"];\n";
                        let tes = List.nth cus 48 in

                        List.iter (output_string oc) (trav tes);
                        fprintf oc "}\n"; close_out oc;
                    end
                end
        | ".debug_line" ->
                  print_endline "Raw dump of debug contents of section .debug_line:\n";
                  while DwarfUtils.peek section_stream != None do
                      let header = DwarfReader.read_line_prog_header section_stream in
                      let ln = DwarfReader.read_line_prog_stmts section_stream header in

                      DwarfPrinter.string_of_lineprog_header header;
                      DwarfPrinter.string_of_lineprg ln
                  done
        | ".debug_abbrev" ->
                begin
                    let abbrev_table_by_offset = DwarfReader.read_abbrev_section section_stream (Hashtbl.create 10) in
                    Hashtbl.iter (fun k v -> Printf.printf "abbrevs for offset 0x%x\n" k;
                                             DwarfPrinter.string_of_abbrev_section v;
                                             Printf.printf "----------------------\n") abbrev_table_by_offset
                end
        | ".debug_frame" | ".debug_ranges" | ".debug_loc" | _ -> print_endline "other sections not supported yet"

    end;

  ) arg_usage
