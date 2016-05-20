open ElfTypes.RAW
open Printf
open Zipper
open DwarfDIE
open DwarfTypes
open Int32

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
                        let tes = List.nth cus 1 in

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
        | ".debug_loc" ->
            let abbrev_section_stream = DwarfUtils.of_string @@ get_section t_original ".debug_abbrev" in
            let info_section_stream = DwarfUtils.of_string @@ get_section t_original ".debug_info" in
            let abbrev_table = DwarfReader.read_abbrev_section abbrev_section_stream (Hashtbl.create 10) in
            let cus = DwarfReader.read_CUs abbrev_table info_section_stream in

            let ds = DwarfUtils.of_string @@ get_section t_original ".debug_str" in
            let string_of_ofs ofs = DwarfUtils.read_null_terminated_string {ds with offset = ref (Int64.to_int ofs)} in

            let ocaml_cu = List.nth cus 1 in

            let rec attr2val target_at ats vals = match ats, vals with
                |(at, _) :: tl1 , (_, (_, v)) :: tl2 -> if target_at == at then [v] else attr2val target_at tl1 tl2
                | ((_, _)::_, [])
                | ([], _::_)
                | [], [] -> []
            in

            let attr_val x v = attr2val x v.die_attributes v.die_attribute_vals in
            let get_name v =
                    match attr_val DW_AT_name v with
                    | [x] -> begin match x with | (OFS_I32 (i)) -> string_of_ofs @@ Int64.of_int32 i
                                          | (OFS_I64 (i)) -> string_of_ofs @@ i
                                          | _ -> failwith "nope" end
                    | [] | _::_::_ -> failwith "malformed lol" in
            let get_loc v =
                    match attr_val DW_AT_location v with
                    | [x] -> begin match x with | (OFS_I32 (i)) -> Int64.of_int32 i
                                          | (OFS_I64 (i)) -> i
                                          | _ -> failwith "nope" end
                    | [] | _::_::_ -> failwith "malformed lol" in
            let curr_spn = ref "" in

            let rec find_subp z lres =
                try
                    let ptr = go_ahead z in
                    let ctree = current_tree ptr in
                    let cval = current_value' ptr in
                    let pv =
                      begin match cval.die_tag with
                        | DW_TAG_subprogram -> begin

                            if List.length cval.die_attributes == 3 then curr_spn := get_name cval;

                            if List.length cval.die_attributes == 5
                            then begin

                                let sp_low_pc = match attr_val DW_AT_low_pc cval with
                                    | [x] -> begin match x with | (OFS_I32 (i)) -> Int64.of_int32 i
                                                          | (OFS_I64 (i)) -> i
                                                          | _ -> failwith "nope" end
                                    | [] | _::_::_ -> failwith "malformed lol"
                                in
                                Zipper.fold_tree (fun x l ->
                                    let res =
                                        match x.die_tag with
                                            | DW_TAG_variable -> [(!curr_spn, sp_low_pc, true, x)]
                                            | DW_TAG_formal_parameter -> [(!curr_spn, sp_low_pc, false, x)]
                                            | _ -> [] in
                                    res @ List.concat l) ctree
                            end
                            else []
                        end
                        | _ -> []
                      end in
                    find_subp ptr (lres @ pv)
                with _ -> lres in

            let pv = find_subp (ocaml_cu, Zipper.Top) [] in
            let pv_map = Hashtbl.create 10 in

            List.iter (fun (spn, sppc, var, atrs) ->
                printf "%s %s : %Lx with %Lx\n" spn (get_name atrs) (get_loc atrs) sppc;
                Hashtbl.add pv_map (get_loc atrs) (spn, (get_name atrs), sppc, var)) pv;

            let locs = List.map
                (fun (_,_,_,atrs) ->
                    DwarfReader.read_locs {section_stream with offset =  ref (Int64.to_int @@ get_loc atrs)}
                ) pv in

            DwarfPrinter.print_locs locs pv_map

        | ".debug_frame" | ".debug_ranges" |  _ -> print_endline "other sections not supported yet"

    end;

  ) arg_usage
