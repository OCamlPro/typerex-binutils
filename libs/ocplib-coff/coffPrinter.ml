(**************************************************************************)
(*                                                                        *)
(*  Copyright 2014 OCamlPro                                               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open CoffTypes.RAW

  let string_of_machine = function
    | IMAGE_FILE_MACHINE_UNKNOWN -> "IMAGE_FILE_MACHINE_UNKNOWN"
    | IMAGE_FILE_MACHINE_AM33 -> "IMAGE_FILE_MACHINE_AM33"
    | IMAGE_FILE_MACHINE_AMD64 -> "IMAGE_FILE_MACHINE_AMD64"
    | IMAGE_FILE_MACHINE_ARM -> "IMAGE_FILE_MACHINE_ARM"
    | IMAGE_FILE_MACHINE_EBC -> "IMAGE_FILE_MACHINE_EBC"
    | IMAGE_FILE_MACHINE_I386 -> "IMAGE_FILE_MACHINE_I386"
    | IMAGE_FILE_MACHINE_IA64 -> "IMAGE_FILE_MACHINE_IA64"
    | IMAGE_FILE_MACHINE_M32R -> "IMAGE_FILE_MACHINE_M32R"
    | IMAGE_FILE_MACHINE_MIPS16 -> "IMAGE_FILE_MACHINE_MIPS16"
    | IMAGE_FILE_MACHINE_MIPSFPU -> "IMAGE_FILE_MACHINE_MIPSFPU"
    | IMAGE_FILE_MACHINE_MIPSFPU16 -> "IMAGE_FILE_MACHINE_MIPSFPU16"
    | IMAGE_FILE_MACHINE_POWERPC -> "IMAGE_FILE_MACHINE_POWERPC"
    | IMAGE_FILE_MACHINE_POWERPCFP -> "IMAGE_FILE_MACHINE_POWERPCFP"
    | IMAGE_FILE_MACHINE_R4000 -> "IMAGE_FILE_MACHINE_R4000"
    | IMAGE_FILE_MACHINE_SH3 -> "IMAGE_FILE_MACHINE_SH3"
    | IMAGE_FILE_MACHINE_SH3DSP -> "IMAGE_FILE_MACHINE_SH3DSP"
    | IMAGE_FILE_MACHINE_SH4 -> "IMAGE_FILE_MACHINE_SH4"
    | IMAGE_FILE_MACHINE_SH5 -> "IMAGE_FILE_MACHINE_SH5"
    | IMAGE_FILE_MACHINE_THUMB -> "IMAGE_FILE_MACHINE_THUMB"
    | IMAGE_FILE_MACHINE_WCEMIPSV2 -> "IMAGE_FILE_MACHINE_WCEMIPSV2"

  let string_of_characteristics = function
    | IMAGE_FILE_RELOCS_STRIPPED -> "IMAGE_FILE_RELOCS_STRIPPED"
    | IMAGE_FILE_EXECUTABLE_IMAGE -> "IMAGE_FILE_EXECUTABLE_IMAGE"
    | IMAGE_FILE_LINE_NUMS_STRIPPED -> "IMAGE_FILE_LINE_NUMS_STRIPPED"
    | IMAGE_FILE_LOCAL_SYMS_STRIPPED -> "IMAGE_FILE_LOCAL_SYMS_STRIPPED"
    | IMAGE_FILE_AGGRESIVE_WS_TRIM -> "IMAGE_FILE_AGGRESIVE_WS_TRIM"
    | IMAGE_FILE_LARGE_ADDRESS_AWARE -> "IMAGE_FILE_LARGE_ADDRESS_AWARE"
    | IMAGE_FILE_BYTES_REVERSED_LO -> "IMAGE_FILE_BYTES_REVERSED_LO"
    | IMAGE_FILE_32BIT_MACHINE -> "IMAGE_FILE_32BIT_MACHINE"
    | IMAGE_FILE_DEBUG_STRIPPED -> "IMAGE_FILE_DEBUG_STRIPPED"
    | IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP -> "IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP"
    | IMAGE_FILE_NET_RUN_FROM_SWAP -> "IMAGE_FILE_NET_RUN_FROM_SWAP"
    | IMAGE_FILE_SYSTEM -> "IMAGE_FILE_SYSTEM"
    | IMAGE_FILE_DLL -> "IMAGE_FILE_DLL"
    | IMAGE_FILE_UP_SYSTEM_ONLY -> "IMAGE_FILE_UP_SYSTEM_ONLY"
    | IMAGE_FILE_BYTES_REVERSED_HI -> "IMAGE_FILE_BYTES_REVERSED_HI"


let string_of_dllCharacteristics = function
  | IMAGE_DLL_CHARACTERISTICS_DYNAMIC_BASE ->
    "IMAGE_DLL_CHARACTERISTICS_DYNAMIC_BASE"
  | IMAGE_DLL_CHARACTERISTICS_FORCE_INTEGRITY ->
    "IMAGE_DLL_CHARACTERISTICS_FORCE_INTEGRITY"
  | IMAGE_DLL_CHARACTERISTICS_NX_COMPAT ->
    "IMAGE_DLL_CHARACTERISTICS_NX_COMPAT"
  | IMAGE_DLLCHARACTERISTICS_NO_ISOLATION ->
    "IMAGE_DLLCHARACTERISTICS_NO_ISOLATION"
  | IMAGE_DLLCHARACTERISTICS_NO_SEH -> "IMAGE_DLLCHARACTERISTICS_NO_SEH"
  | IMAGE_DLLCHARACTERISTICS_NO_BIND -> "IMAGE_DLLCHARACTERISTICS_NO_BIND"
  | IMAGE_DLLCHARACTERISTICS_WDM_DRIVER ->
    "IMAGE_DLLCHARACTERISTICS_WDM_DRIVER"
  | IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE ->
    "IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE"

let string_of_section_flag = function
  | IMAGE_SCN_TYPE_NO_PAD -> "IMAGE_SCN_TYPE_NO_PAD"
  | IMAGE_SCN_CNT_CODE -> "IMAGE_SCN_CNT_CODE"
  | IMAGE_SCN_CNT_INITIALIZED_DATA -> "IMAGE_SCN_CNT_INITIALIZED_DATA"
  | IMAGE_SCN_CNT_UNINITIALIZED_DATA -> "IMAGE_SCN_CNT_UNINITIALIZED_DATA"
  | IMAGE_SCN_LNK_OTHER -> "IMAGE_SCN_LNK_OTHER"
  | IMAGE_SCN_LNK_INFO -> "IMAGE_SCN_LNK_INFO"
  | IMAGE_SCN_LNK_REMOVE -> "IMAGE_SCN_LNK_REMOVE"
  | IMAGE_SCN_LNK_COMDAT -> "IMAGE_SCN_LNK_COMDAT"
  | IMAGE_SCN_NO_DEFER_SPEC_EXC -> "IMAGE_SCN_NO_DEFER_SPEC_EXC"
  | IMAGE_SCN_GPREL -> "IMAGE_SCN_GPREL"
  | IMAGE_SCN_MEM_FARDATA -> "IMAGE_SCN_MEM_FARDATA"
  | IMAGE_SCN_MEM_PURGEABLE -> "IMAGE_SCN_MEM_PURGEABLE"
  | IMAGE_SCN_MEM_16BIT -> "IMAGE_SCN_MEM_16BIT"
  | IMAGE_SCN_MEM_LOCKED -> "IMAGE_SCN_MEM_LOCKED"
  | IMAGE_SCN_MEM_PRELOAD -> "IMAGE_SCN_MEM_PRELOAD"
  | IMAGE_SCN_ALIGN_1BYTES -> "IMAGE_SCN_ALIGN_1BYTES"
  | IMAGE_SCN_ALIGN_2BYTES -> "IMAGE_SCN_ALIGN_2BYTES"
  | IMAGE_SCN_ALIGN_4BYTES -> "IMAGE_SCN_ALIGN_4BYTES"
  | IMAGE_SCN_ALIGN_8BYTES -> "IMAGE_SCN_ALIGN_8BYTES"
  | IMAGE_SCN_ALIGN_16BYTES -> "IMAGE_SCN_ALIGN_16BYTES"
  | IMAGE_SCN_ALIGN_32BYTES -> "IMAGE_SCN_ALIGN_32BYTES"
  | IMAGE_SCN_ALIGN_64BYTES -> "IMAGE_SCN_ALIGN_64BYTES"
  | IMAGE_SCN_ALIGN_128BYTES -> "IMAGE_SCN_ALIGN_128BYTES"
  | IMAGE_SCN_ALIGN_256BYTES -> "IMAGE_SCN_ALIGN_256BYTES"
  | IMAGE_SCN_ALIGN_512BYTES -> "IMAGE_SCN_ALIGN_512BYTES"
  | IMAGE_SCN_ALIGN_1024BYTES -> "IMAGE_SCN_ALIGN_1024BYTES"
  | IMAGE_SCN_ALIGN_2048BYTES -> "IMAGE_SCN_ALIGN_2048BYTES"
  | IMAGE_SCN_ALIGN_4096BYTES -> "IMAGE_SCN_ALIGN_4096BYTES"
  | IMAGE_SCN_ALIGN_8192BYTES -> "IMAGE_SCN_ALIGN_8192BYTES"
  | IMAGE_SCN_LNK_NRELOC_OVFL -> "IMAGE_SCN_LNK_NRELOC_OVFL"
  | IMAGE_SCN_MEM_DISCARDABLE -> "IMAGE_SCN_MEM_DISCARDABLE"
  | IMAGE_SCN_MEM_NOT_CACHED -> "IMAGE_SCN_MEM_NOT_CACHED"
  | IMAGE_SCN_MEM_NOT_PAGED -> "IMAGE_SCN_MEM_NOT_PAGED"
  | IMAGE_SCN_MEM_SHARED -> "IMAGE_SCN_MEM_SHARED"
  | IMAGE_SCN_MEM_EXECUTE -> "IMAGE_SCN_MEM_EXECUTE"
  | IMAGE_SCN_MEM_READ -> "IMAGE_SCN_MEM_READ"
  | IMAGE_SCN_MEM_WRITE -> "IMAGE_SCN_MEM_WRITE"

let string_of_pe_kind = function
  | PE32 -> "PE32"
  | PE64 -> "PE64 (* PE32+ *)"

let string_of_image_directory_index = function
  | 0 -> "ExportTable"
  | 1 -> "ImportTable"
  | 2 -> "ResourceTable"
  | 3 -> "ExceptionTable"
  | 4 -> "CertificateTable"
  | 5 -> "BaseRelocationTable"
  | 6 -> "Debug"
  | 7 -> "Architecture"
  | 8 -> "GlobalPtr"
  | 9 -> "TLSTable"
  | 10 -> "LoadConfigTable"
  | 11 -> "BoundImport"
  | 12 -> "IAT"
  | 13 -> "DelayImportDescriptor"
  | 14 -> "CLRRuntimeHeader"
  | 15 -> "ZERO"
  | _ -> "???"


module RAW = struct


  let optional_coff_header b indent h =
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s pe_kind = %s;\n" indent (string_of_pe_kind h.pe_kind);
    Printf.bprintf b "%s majorLinkerVersion = %d;\n" indent h.majorLinkerVersion;
    Printf.bprintf b "%s minorLinkerVersion = %d;\n" indent h.minorLinkerVersion;
    Printf.bprintf b "%s sizeOfCode = %d;\n" indent h.sizeOfCode;
    Printf.bprintf b "%s sizeOfInitializedData = %d;\n" indent h.sizeOfInitializedData;
    Printf.bprintf b "%s sizeOfUninitializedData = %d;\n" indent h.sizeOfUninitializedData;
    Printf.bprintf b "%s addressOfEntryPoint = %d;\n" indent h.addressOfEntryPoint;
    Printf.bprintf b "%s baseOfCode = %d;\n" indent h.baseOfCode;
    Printf.bprintf b "%s baseOfData = %d;\n" indent h.baseOfData;
    Printf.bprintf b "%s imageBase = 0x%Lx; (* %Ld *)\n" indent h.imageBase h.imageBase;
    Printf.bprintf b "%s sectionAlignment = %d;\n" indent h.sectionAlignment;
    Printf.bprintf b "%s fileAlignment = %d;\n" indent h.fileAlignment;
    Printf.bprintf b "%s majorOperatingSystemVersion = %d;\n" indent h.majorOperatingSystemVersion;
    Printf.bprintf b "%s minorOperatingSystemVersion = %d;\n" indent h.minorOperatingSystemVersion;
    Printf.bprintf b "%s majorImageVersion = %d;\n" indent h.majorImageVersion;
    Printf.bprintf b "%s minorImageVersion = %d;\n" indent h.minorImageVersion;
    Printf.bprintf b "%s majorSubsystemVersion = %d;\n" indent h.majorSubsystemVersion;
    Printf.bprintf b "%s minorSubsystemVersion = %d;\n" indent h.minorSubsystemVersion;
    Printf.bprintf b "%s win32VersionValue = %d;\n" indent h.win32VersionValue;
    Printf.bprintf b "%s sizeOfImage = %d;\n" indent h.sizeOfImage;
    Printf.bprintf b "%s sizeOfHeaders = %d;\n" indent h.sizeOfHeaders;
    Printf.bprintf b "%s checkSum = %S;\n" indent h.checkSum;
    Printf.bprintf b "%s subsystem = %d;\n" indent h.subsystem;
    Printf.bprintf b "%s dllCharacteristics = %d;\n" indent h.dllCharacteristics;
    Printf.bprintf b "%s sizeOfStackReserve = 0x%Lx; (* %Ld *)\n" indent h.sizeOfStackReserve h.sizeOfStackReserve;
    Printf.bprintf b "%s sizeOfStackCommit = 0x%Lx; (* %Ld *)\n" indent h.sizeOfStackCommit h.sizeOfStackCommit;
    Printf.bprintf b "%s sizeOfHeapReserve = 0x%Lx; (* %Ld *)\n" indent h.sizeOfHeapReserve h.sizeOfHeapReserve;
    Printf.bprintf b "%s sizeOfHeapCommit = 0x%Lx; (* %Ld *)\n" indent h.sizeOfHeapCommit h.sizeOfHeapCommit;
    Printf.bprintf b "%s loaderFlags = %d;\n" indent h.loaderFlags;
    Printf.bprintf b "%s numberOfRvaAndSizes = %d;\n" indent h.numberOfRvaAndSizes;
    Printf.bprintf b "%s directories = [|\n" indent;
    Array.iteri (fun i im ->
      Printf.bprintf b "%s  { (* %s *)\n" indent
        (string_of_image_directory_index i);
      Printf.bprintf b "%s    image_rva = 0x%Lx; (* %Ld *)\n" indent im.image_rva im.image_rva;
      Printf.bprintf b "%s    image_size = 0x%Lx; (* %Ld *)\n" indent im.image_size im.image_size;
      Printf.bprintf b "%s  };\n" indent;
    ) h.directories;
    Printf.bprintf b "%s    |];\n" indent;
    Printf.bprintf b "%s}" indent


  let coff_header b indent h =
    let indent2 = indent ^ "  " in
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s machine = %s;\n" indent (string_of_machine h.machine);
    Printf.bprintf b "%s numberOfSections = %d;\n" indent h.numberOfSections;
    Printf.bprintf b "%s timeDateStamp = %Ld;\n" indent h.timeDateStamp;
    Printf.bprintf b "%s pointerToSymbolTable = %d;\n" indent h.pointerToSymbolTable;
    Printf.bprintf b "%s numberOfSymbols = %d;\n" indent h.numberOfSymbols;
    Printf.bprintf b "%s sizeOfOptionalHeader = %d;\n" indent h.sizeOfOptionalHeader;
    Printf.bprintf b "%s characteristics = [ %s];\n" indent
      (String.concat "; "
         (List.map string_of_characteristics h.characteristics));
    begin match h.optionalHeader with
      None ->
        Printf.bprintf b "%s optionalHeader = None;\n" indent;
      | Some h ->
        Printf.bprintf b "%s optionalHeader = Some" indent;
        optional_coff_header b indent2 h;
        Printf.bprintf b ";\n";
    end;
    Printf.bprintf b "%s}" indent

  let section_header b indent s =
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s section_name = %S;\n" indent s.section_name;
   Printf.bprintf b "%s virtualSize = 0x%Lx; (* %Ld *)\n" indent s.virtualSize s.virtualSize;
   Printf.bprintf b "%s virtualAddress = 0x%Lx; (* %Ld *)\n" indent s.virtualAddress s.virtualAddress;
   Printf.bprintf b "%s sizeOfRawData = 0x%Lx; (* %Ld *)\n" indent s.sizeOfRawData s.sizeOfRawData;
   Printf.bprintf b "%s pointerToRawData = 0x%Lx; (* %Ld *)\n" indent s.pointerToRawData s.pointerToRawData;
   Printf.bprintf b "%s pointerToRelocations = 0x%Lx; (* %Ld *)\n" indent s.pointerToRelocations s.pointerToRelocations;
   Printf.bprintf b "%s pointerToLinenumbers = 0x%Lx; (* %Ld *)\n" indent s.pointerToLinenumbers s.pointerToLinenumbers;
   Printf.bprintf b "%s numberOfRelocations = %d;\n" indent s.numberOfRelocations;
   Printf.bprintf b "%s numberOfLinenumbers = %d;\n" indent s.numberOfLinenumbers;
   Printf.bprintf b "%s sectionFlags = [%s];\n" indent
   (String.concat "; " (List.map string_of_section_flag s.sectionFlags));
    Printf.bprintf b "%s}" indent

  let symbol b indent s =
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s sym_name = %S;\n" indent s.sym_name;
    Printf.bprintf b "%s sym_value = 0x%Lx; (* %Ld *)\n" indent s.sym_value s.sym_value;
    Printf.bprintf b "%s sym_sectionNumber = %d;\n" indent s.sym_sectionNumber;
    Printf.bprintf b "%s sym_type = %d;\n" indent s.sym_type;
    Printf.bprintf b "%s sym_storageClass = %d;\n" indent s.sym_storageClass;
    Printf.bprintf b "%s sym_numberOfAuxSymbols = %d;\n" indent
      s.sym_numberOfAuxSymbols;
    Printf.bprintf b "%s}" indent



  let object_file b indent ~with_symbols p =
    let indent2 = indent ^ "  " in
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s obj_base = %d;\n" indent p.obj_base ;
    Printf.bprintf b "%s obj_size = %d;\n" indent p.obj_size;
    Printf.bprintf b "%s obj_coff_header = " indent;
    coff_header b indent2 p.obj_coff_header;
    Printf.bprintf b ";\n";
    Printf.bprintf b "%s obj_strtab =  _; (* len = %d *)\n"
      indent (String.length p.obj_strtab);
    Printf.bprintf b "%s obj_sections = [|\n" indent;
    Array.iteri (fun i s ->
      Printf.bprintf b "%s  " indent;
      section_header b indent2 s;
      Printf.bprintf b ";\n";
    ) p.obj_sections;
    if with_symbols then begin
      Printf.bprintf b "%s obj_symbols = [|\n" indent;
      Array.iteri (fun i s ->
        Printf.bprintf b "%s  " indent;
        symbol b indent2 s;
        Printf.bprintf b ";\n";
      ) p.obj_symbols;
      Printf.bprintf b "%s   |];\n" indent;
    end else
      Printf.bprintf b "%s obj_symbols = [| _ (* %d symbols *) |];\n" indent
    (Array.length p.obj_symbols);
    Printf.bprintf b "%s}" indent

  let pe_file indent ?(with_symbols=true) p =
    let b = Buffer.create 1000 in
    let indent2 = indent ^ "  " in
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s pe_content = _;\n" indent ;
    Printf.bprintf b "%s pe_size = %d;\n" indent p.pe_size;
    Printf.bprintf b "%s pe_stub = %S;\n" indent p.pe_stub;
    Printf.bprintf b "%s pe_pos = %d;\n" indent p.pe_pos;
    Printf.bprintf b "%s pe_obj = " indent;
    object_file b indent2 ~with_symbols p.pe_obj;
    Printf.bprintf b ";\n";
    Printf.bprintf b "%s}" indent;
    Buffer.contents b

  let object_file indent ?(with_symbols=true) p =
    let b = Buffer.create 1000 in
    object_file b indent ~with_symbols p;
    Buffer.contents b

end
