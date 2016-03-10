open ElfTypes.RAW

let print_dwarf = ref false

let arg_list = Arg.align [
    "-dw", Arg.Set print_dwarf, " DWARF output";
  ]

let arg_usage =
  Printf.sprintf "%s [OPTIONS] FILES" (Filename.basename Sys.argv.(0))

(*let string_of_addr v =*)
  (*Printf.sprintf "0x%LxL (* %Ld *)" v v*)

  (*let section_header b indent sh =*)
    (*Printf.bprintf b "{\n";*)
    (*Printf.bprintf b "%s sh_name = %LdL\n" indent sh.sh_name;*)
    (*Printf.bprintf b "%s sh_addr = %s;\n" indent (string_of_addr sh.sh_addr);*)
    (*Printf.bprintf b "%s sh_offset = %Ld;\n" indent sh.sh_offset;*)
    (*Printf.bprintf b "%s sh_size = %Ld;\n" indent sh.sh_size;*)
    (*Printf.bprintf b "%s sh_link = %Ld;\n" indent sh.sh_link;*)
    (*Printf.bprintf b "%s sh_info = %Ld;\n" indent sh.sh_info;*)
    (*Printf.bprintf b "%s sh_addralign = %Ld;\n" indent sh.sh_addralign;*)
    (*Printf.bprintf b "%s sh_entsize = %Ld;\n" indent sh.sh_entsize;*)
    (*Printf.bprintf b "%s}" indent*)

  (*let section b indent s =*)
    (*Printf.bprintf b "{\n";*)
    (*Printf.bprintf b "%s section_name = %S;\n" indent s.section_name;*)
    (*Printf.bprintf b "%s section_content = string[%d];\n" indent*)
      (*(String.length s.section_content);*)
    (*Printf.bprintf b "%s section_header = " indent;*)
    (*section_header b (indent ^ "  ") s.section_header;*)
    (*Printf.bprintf b ";\n";*)
    (*Printf.bprintf b "%s}" indent*)

let _ =
  Arg.parse arg_list (fun file ->

    if !print_dwarf then begin
        Printf.printf "DWARF\n";

        let raw = ElfReader.RAW.read file in

        let regex = (Str.regexp "debug") in
        let filter_debug_sections s_name = Str.string_match regex s_name 1 in
        let b = Buffer.create 1000 in
        let indent = "    " in

        Array.iteri (fun i s ->
            if String.length s.section_name > 0 then begin
                if filter_debug_sections s.section_name
                then begin
                    Printf.printf "%s\n" s.section_name;
                    let bu = s.section_content in
                    Xxd.output_lines bu (Buffer.create 16) (Buffer.create 16) 0 (String.length bu)
                    (*Printf.bprintf b "%s   (* section %d *) " indent i;*)
                    (*section b (indent ^ "     ") s;*)
                    (*Printf.bprintf b ";\n";*)
                end
            end;
        ) raw.elf_sections;

        Printf.printf "%s\n" (Buffer.contents b);
    end;

  ) arg_usage
