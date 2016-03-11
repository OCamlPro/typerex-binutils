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
        let section_stream = Stream.of_string target_section in

        if String.compare !single_section ".debug_info" == 0 then
        DwarfReader.read_section_header section_stream;

        if !hex_flag then begin
            dump_hex target_section;
        end
    end;

  ) arg_usage
