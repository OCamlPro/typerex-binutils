(**************************************************************************)
(*                                                                        *)
(*  Copyright 2014 OCamlPro                                               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

let arg_list = Arg.align []
let arg_usage =
  Printf.sprintf "%s [OPTIONS] FILES" (Filename.basename Sys.argv.(0))

let with_symbols = ref false

let read_library file =
  Printf.printf "File %S...\n%!" file;
  let module L = CoffReader.Library in
  let lib = L.read file in
  Array.iter (fun m ->
    Printf.printf "  %10d...%s (%s)\n%!" m.L.mber_pos m.L.mber_name
      (L.string_of_kind m.L.mber_kind);
  ) lib.L.lib_members;
  let _symbols1 = L.read_linker1 lib in
  Printf.printf "(linker1)%!";
  begin
    match L.read_linker2 lib with
    | Some _ -> Printf.printf "(linker2)%!";
    | None ->   Printf.printf "(no linker2)%!";
  end;
  let with_symbols = !with_symbols in
  L.iter_objects (fun lib i obj ->
    Printf.printf " object %d %s -> %s\n%!"
      i lib.L.lib_members.(i).L.mber_name
      (CoffPrinter.RAW.object_file "   " ~with_symbols obj)
  ) lib;
  close_in lib.L.lib_ic;
  Printf.printf "... OK\n%!";
  ()

let read_program file =
  let module P = CoffReader.PEFile in
  let p = P.read file in
  let s = CoffPrinter.RAW.pe_file "  " p in
  Printf.printf "%S = %s\n%!" file s;
  ()

let read_object file =
  let module P = CoffReader.Object in
  let p = P.read file in
  let s = CoffPrinter.RAW.object_file "  " p in
  Printf.printf "%S = %s\n%!" file s;
  ()

let _ =
  Arg.parse arg_list (fun file ->
    if Filename.check_suffix file ".a" ||
       Filename.check_suffix file ".lib" then
      read_library file
    else
    if Filename.check_suffix file ".exe" ||
       Filename.check_suffix file ".dll" ||
       Filename.check_suffix file ".sys"
    then
      read_program file
    else
    if
      Filename.check_suffix file ".o" ||
      Filename.check_suffix file ".obj"
    then
      read_object file
    else begin
      Printf.eprintf "Error: don't know what to do with %S\n%!" file;
      exit 2
    end
  ) arg_usage
