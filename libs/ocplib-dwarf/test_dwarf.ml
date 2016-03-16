open ElfTypes.RAW

type t = (string, string) Hashtbl.t;;

let hex_flag = ref false
let single_section = ref ""

let arg_list = Arg.align [
    "-xxd", Arg.Set hex_flag, " output hex dump of target section";
    "--section", Arg.String (fun s -> single_section := s), " target section";
  ]

let arg_usage =
  Printf.sprintf "%s [OPTIONS] FILES" (Filename.basename Sys.argv.(0))

let dump_hex s =
  Xxd.output_lines s (Buffer.create 16) (Buffer.create 16) 0 (String.length s)

let is_string_empty s = (s = "")

let _ =
  Arg.parse arg_list (fun file ->

    let raw = ElfReader.RAW.read file in

    let regex = (Str.regexp "debug") in
    let filter_debug_sections s_name = Str.string_match regex s_name 1 in

    let t_original : t = Hashtbl.create 10 in
    let abbrev_table_by_offset = ref (Hashtbl.create 20) in

    Array.iteri (fun i s ->
        if String.length s.section_name > 0 then begin
            if filter_debug_sections s.section_name
            then begin
                Hashtbl.add t_original s.section_name s.section_content;
            end
        end;
    ) raw.elf_sections;

    if is_string_empty !single_section then begin
        Printf.printf "available sections : \n";
        Hashtbl.iter (fun k v -> Printf.printf "%s\n" k) t_original
    end
    else begin
        let target_section =
            try
                Hashtbl.find t_original !single_section
            with Not_found -> Printf.printf "error : section %s not found\n" !single_section; exit 1 in
        let section_stream = Stream_in.of_string target_section in

        if !hex_flag then begin
            dump_hex target_section;
            exit 1
        end;

        if String.compare !single_section ".debug_info" == 0 then
            DwarfReader.read_CUs section_stream;

        if String.compare !single_section ".debug_abbrev" == 0 then
            abbrev_table_by_offset := DwarfReader.read_abbrev_section section_stream (Hashtbl.create 10);
            Hashtbl.iter (fun k v -> Printf.printf "abbrevs for offset %d\n" k;
                                     DwarfPrinter.string_of_abbrev_section v;
                                     Printf.printf "----------------------\n") !abbrev_table_by_offset

    end;

  ) arg_usage
