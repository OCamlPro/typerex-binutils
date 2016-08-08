(**************************************************************************)
(*                                                                        *)
(*  Copyright 2014 OCamlPro                                               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

module RAW : sig


  type machine =
    | IMAGE_FILE_MACHINE_UNKNOWN
    | IMAGE_FILE_MACHINE_AM33
    | IMAGE_FILE_MACHINE_AMD64
    | IMAGE_FILE_MACHINE_ARM
    | IMAGE_FILE_MACHINE_EBC
    | IMAGE_FILE_MACHINE_I386
    | IMAGE_FILE_MACHINE_IA64
    | IMAGE_FILE_MACHINE_M32R
    | IMAGE_FILE_MACHINE_MIPS16
    | IMAGE_FILE_MACHINE_MIPSFPU

    | IMAGE_FILE_MACHINE_MIPSFPU16
    | IMAGE_FILE_MACHINE_POWERPC
    | IMAGE_FILE_MACHINE_POWERPCFP
    | IMAGE_FILE_MACHINE_R4000
    | IMAGE_FILE_MACHINE_SH3
    | IMAGE_FILE_MACHINE_SH3DSP
    | IMAGE_FILE_MACHINE_SH4
    | IMAGE_FILE_MACHINE_SH5
    | IMAGE_FILE_MACHINE_THUMB
    | IMAGE_FILE_MACHINE_WCEMIPSV2

  type characteristics =
    | IMAGE_FILE_RELOCS_STRIPPED
    | IMAGE_FILE_EXECUTABLE_IMAGE
    | IMAGE_FILE_LINE_NUMS_STRIPPED
    | IMAGE_FILE_LOCAL_SYMS_STRIPPED
    | IMAGE_FILE_AGGRESIVE_WS_TRIM
    | IMAGE_FILE_LARGE_ADDRESS_AWARE
    | IMAGE_FILE_BYTES_REVERSED_LO
    | IMAGE_FILE_32BIT_MACHINE
    | IMAGE_FILE_DEBUG_STRIPPED
    | IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP
    | IMAGE_FILE_NET_RUN_FROM_SWAP
    | IMAGE_FILE_SYSTEM
    | IMAGE_FILE_DLL
    | IMAGE_FILE_UP_SYSTEM_ONLY
    | IMAGE_FILE_BYTES_REVERSED_HI

  type subsystem =
    | IMAGE_SUBSYSTEM_UNKNOWN
    | IMAGE_SUBSYSTEM_NATIVE (* 1 device drivers and native Windows processes *)
    | IMAGE_SUBSYSTEM_WINDOWS_GUI (* 2 Windows graphical interface *)
    | IMAGE_SUBSYSTEM_WINDOWS_CUI (* 3 Windows character interface *)
    | IMAGE_SUBSYSTEM_POSIX_CUI  (* 4 The POSIX character subsystem *)
    | IMAGE_SUBSYSTEM_WINDOWS_CE_GUI (* 9 Windows CE *)
    | IMAGE_SUBSYSTEM_EFI_APPLICATION (* 10 EFI application *)
    | IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER (* 11 EPI driver *)
    | IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER (* 12 EFI driver with runtime srvs *)
    | IMAGE_SUBSYSTEM_EFI_ROM (* 13 EFI ROM *)
    | IMAGE_SUBSYSTEM_XBOX  (* 14 *)

  type dllCharacteristics =
    | IMAGE_DLL_CHARACTERISTICS_DYNAMIC_BASE
    | IMAGE_DLL_CHARACTERISTICS_FORCE_INTEGRITY
    | IMAGE_DLL_CHARACTERISTICS_NX_COMPAT
    | IMAGE_DLLCHARACTERISTICS_NO_ISOLATION
    | IMAGE_DLLCHARACTERISTICS_NO_SEH
    | IMAGE_DLLCHARACTERISTICS_NO_BIND
    | IMAGE_DLLCHARACTERISTICS_WDM_DRIVER
    | IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE

  type section_flag =

| IMAGE_SCN_TYPE_NO_PAD

| IMAGE_SCN_CNT_CODE
| IMAGE_SCN_CNT_INITIALIZED_DATA
| IMAGE_SCN_CNT_UNINITIALIZED_DATA

| IMAGE_SCN_LNK_OTHER
| IMAGE_SCN_LNK_INFO
| IMAGE_SCN_LNK_REMOVE
| IMAGE_SCN_LNK_COMDAT
| IMAGE_SCN_NO_DEFER_SPEC_EXC
| IMAGE_SCN_GPREL
| IMAGE_SCN_MEM_FARDATA
| IMAGE_SCN_MEM_PURGEABLE
| IMAGE_SCN_MEM_16BIT
| IMAGE_SCN_MEM_LOCKED
| IMAGE_SCN_MEM_PRELOAD

| IMAGE_SCN_ALIGN_1BYTES
| IMAGE_SCN_ALIGN_2BYTES
| IMAGE_SCN_ALIGN_4BYTES
| IMAGE_SCN_ALIGN_8BYTES
| IMAGE_SCN_ALIGN_16BYTES
| IMAGE_SCN_ALIGN_32BYTES
| IMAGE_SCN_ALIGN_64BYTES
| IMAGE_SCN_ALIGN_128BYTES
| IMAGE_SCN_ALIGN_256BYTES
| IMAGE_SCN_ALIGN_512BYTES
| IMAGE_SCN_ALIGN_1024BYTES
| IMAGE_SCN_ALIGN_2048BYTES
| IMAGE_SCN_ALIGN_4096BYTES
| IMAGE_SCN_ALIGN_8192BYTES
| IMAGE_SCN_LNK_NRELOC_OVFL
| IMAGE_SCN_MEM_DISCARDABLE
| IMAGE_SCN_MEM_NOT_CACHED
| IMAGE_SCN_MEM_NOT_PAGED
| IMAGE_SCN_MEM_SHARED
| IMAGE_SCN_MEM_EXECUTE
| IMAGE_SCN_MEM_READ
| IMAGE_SCN_MEM_WRITE

(*

| IMAGE_REL_BASED_ABSOLUTE              0
| IMAGE_REL_BASED_HIGH                  1
| IMAGE_REL_BASED_LOW                   2
| IMAGE_REL_BASED_HIGHLOW               3
| IMAGE_REL_BASED_HIGHADJ               4
| IMAGE_REL_BASED_MIPS_JMPADDR          5
| IMAGE_REL_BASED_SECTION               6
| IMAGE_REL_BASED_REL32                 7


| IMAGE_RESOURCE_NAME_IS_STRING        0x80000000
| IMAGE_RESOURCE_DATA_IS_DIRECTORY     0x80000000
          *)




  type image_directory = {
    image_rva : int64;
    image_size : int64;
  }

  type pe_kind = PE32 | PE64 (* PE32+ :-) *)

  type optional_coff_header = {
    pe_kind : pe_kind;
    majorLinkerVersion : int;
    minorLinkerVersion : int;
    sizeOfCode : int;
    sizeOfInitializedData : int;
    sizeOfUninitializedData : int;
    addressOfEntryPoint : int;
    baseOfCode : int;
    baseOfData : int;
    imageBase : int64;
    sectionAlignment : int;
    fileAlignment : int;
    majorOperatingSystemVersion : int;
    minorOperatingSystemVersion : int;
    majorImageVersion : int;
    minorImageVersion : int;
    majorSubsystemVersion : int;
    minorSubsystemVersion : int;
    win32VersionValue : int;
    sizeOfImage : int;
    sizeOfHeaders : int;
    checkSum : string;
    subsystem : int;
    dllCharacteristics : int;
    sizeOfStackReserve : int64;
    sizeOfStackCommit : int64;
    sizeOfHeapReserve : int64;
    sizeOfHeapCommit : int64;
    loaderFlags : int;
    numberOfRvaAndSizes : int;
    directories : image_directory array;
  }

  type coff_header =
    { machine : machine;
      numberOfSections : int;
      timeDateStamp : int64;
      pointerToSymbolTable : int;
      numberOfSymbols : int;
      sizeOfOptionalHeader : int;
      characteristics : characteristics list;
      optionalHeader : optional_coff_header option;
    }

  type section_header = {
    section_name : string;
    virtualSize : int64;
    virtualAddress : int64;
    sizeOfRawData : int64;
    pointerToRawData : int64;
    pointerToRelocations : int64;
    pointerToLinenumbers : int64;
    numberOfRelocations : int;
    numberOfLinenumbers : int;
    sectionFlags : section_flag list;
  }

  type symbol = {
    sym_name : string;
    sym_value : int64;
    sym_sectionNumber : int;
    sym_type : int;
    sym_storageClass : int;
    sym_numberOfAuxSymbols : int;
  }

  type object_file = {
    obj_base : int;
    obj_size : int;
    obj_coff_header : coff_header;
    obj_strtab : string;
    obj_sections : section_header array;
    obj_symbols : symbol array;
  }

  type 'content pe_file = {
    pe_content : 'content;
    pe_size : int;
    pe_stub : string;
    pe_pos : int;
    pe_obj : object_file;
  }



end


