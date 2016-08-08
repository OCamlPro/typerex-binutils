(**************************************************************************)
(*                                                                        *)
(*  Copyright 2014 OCamlPro                                               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open StringCompat
open ElfTypes
open ElfUtils

let debug = ref false

let get_string0 s pos =
  try
    let pos2 = String.index_from s pos '\000' in
    String.sub s pos (pos2-pos)
  with Not_found ->
    Printf.eprintf "Error:get_string0(%S,%d) failed\n%!" s pos;
    exit 2

let type_of_int v =
  match Int64.to_int v with
  | 0 -> ET_NONE
  | 1 -> ET_REL
  | 2 -> ET_EXEC
  | 3 -> ET_DYN
  | 4 -> ET_CORE
  | n ->
    if n >= 0xff00 && n <= 0xffff then
      ET_PROC (n - 0xff00)
    else
      if n >= 0xfe00 && n <= 0xfeff then
        ET_OS (n - 0xfe00)
      else
        ET_NUM n

let to_bits v =
  let bits = ref [] in
  let bit = ref 1 in
  for _i = 0 to 63 do
    if v land !bit <> 0 then bits := !bit :: !bits;
    bit := !bit lsl 1
  done;
  !bits

let section_header_flag_of_int v =
  match v with
    1 -> SHF_WRITE
  | 2 -> SHF_ALLOC
  | 4 -> SHF_EXECINSTR
  | n -> SHF_NUM n

let section_header_flags_of_int en v =
  let bits = to_bits (Int64.to_int v) in
  List.map section_header_flag_of_int bits

let file_class_of_int v =
  match v with
  | 0 -> ELFCLASSNONE
  | 1 -> ELFCLASS32
  | 2 -> ELFCLASS64
  | n -> ELFCLASSNUM n


let data_encoding_of_int = function
  | 0 -> ELFDATANONE
  | 1 -> ELFDATA2LSB
  | 2 -> ELFDATA2MSB
  | n -> ELFDATANUM n

let osabi_of_int = function
  |  0   -> ELFOSABI_SYSV
  |  1   -> ELFOSABI_HPUX
  |  2   -> ELFOSABI_NETBSD
  |  3   -> ELFOSABI_LINUX
  |  6   -> ELFOSABI_SOLARIS
  |  7   -> ELFOSABI_AIX
  |  8   -> ELFOSABI_IRIX
  |  9   -> ELFOSABI_FREEBSD
  |  10  -> ELFOSABI_TRU64
  |  11  -> ELFOSABI_MODESTO
  |  12  -> ELFOSABI_OPENBSD
  |  13  -> ELFOSABI_OPENVMS
  |  14  -> ELFOSABI_NSK
  |  15  -> ELFOSABI_AROS
  |  97  -> ELFOSABI_ARM
  |  255 -> ELFOSABI_STANDALONE
  |  n   -> ELFOSABI_NUM n

let machine_of_int v =
  match Int64.to_int v with
    0    -> EM_NONE
  | 1    -> EM_M32
  | 2    -> EM_SPARC
  | 3    -> EM_386
  | 4    -> EM_68K
  | 5    -> EM_88K
  | 6    -> EM_486
  | 7    -> EM_860
  | 8    -> EM_MIPS
  | 9    -> EM_S370
  | 10   -> EM_MIPS_RS3_LE
  | 11   -> EM_SPARC64
  | 15   -> EM_PARISC
  | 17   -> EM_VPP500
  | 18   -> EM_SPARC32PLUS
  | 19   -> EM_960
  | 20   -> EM_PPC
  | 21   -> EM_PPC64
  | 22   -> EM_S390
  | 23   -> EM_SPU
  | 36   -> EM_V800
  | 37   -> EM_FR20
  | 38   -> EM_RH32
  | 39   -> EM_RCE
  | 40   -> EM_ARM
  | 41   -> EM_ALPHA
  | 42   -> EM_SH
  | 43   -> EM_SPARCV9
  | 44   -> EM_TRICORE
  | 45   -> EM_ARC
  | 46   -> EM_H8_300
  | 47   -> EM_H8_300H
  | 48   -> EM_H8S
  | 49   -> EM_H8_500
  | 50   -> EM_IA_64
  | 51   -> EM_MIPS_X
  | 52   -> EM_COLDFIRE
  | 53   -> EM_68HC12
  | 54   -> EM_MMA
  | 55   -> EM_PCP
  | 56   -> EM_NCPU
  | 57   -> EM_NDR1
  | 58   -> EM_STARCORE
  | 59   -> EM_ME16
  | 60   -> EM_ST100
  | 61   -> EM_TINYJ
  | 62   -> EM_X86_64
  | 63   -> EM_PDSP
  | 66   -> EM_FX66
  | 67   -> EM_ST9PLUS
  | 68   -> EM_ST7
  | 69   -> EM_68HC16
  | 70   -> EM_68HC11
  | 71   -> EM_68HC08
  | 72   -> EM_68HC05
  | 73   -> EM_SVX
  | 74   -> EM_ST19
  | 75   -> EM_VAX
  | 76   -> EM_CRIS
  | 77   -> EM_JAVELIN
  | 78   -> EM_FIREPATH
  | 79   -> EM_ZSP
  | 80   -> EM_MMIX
  | 81   -> EM_HUANY
  | 82   -> EM_PRISM
  | 83   -> EM_AVR
  | 84   -> EM_FR30
  | 85   -> EM_D10V
  | 86   -> EM_D30V
  | 87   -> EM_V850
  | 88   -> EM_M32R
  | 89   -> EM_MN10300
  | 90   -> EM_MN10200
  | 91   -> EM_PJ
  | 92   -> EM_OPENRISC
  | 93   -> EM_ARC_A5
  | 94   -> EM_XTENSA
  | 95   -> EM_VIDEOCORE
  | 96   -> EM_TMM_GPP
  | 97   -> EM_NS32K
  | 98   -> EM_TPC
  | 99   -> EM_SNP1K
  | 100  -> EM_ST200
  | 101  -> EM_IP2K
  | 102  -> EM_MAX
  | 103  -> EM_CR
  | 104  -> EM_F2MC16
  | 105  -> EM_MSP430
  | 106  -> EM_BLACKFIN
  | 107  -> EM_SE_C33
  | 108  -> EM_SEP
  | 109  -> EM_ARCA
  | 110  -> EM_UNICORE
  | n -> EM_NUM n

let segment_type_of_int = function
  | 0L -> PT_NULL
  | 1L -> PT_LOAD
  | 2L -> PT_DYNAMIC        (* 2 : dynamic linking tables *)
  | 3L -> PT_INTERP         (* 3 : program interpreter path name *)
  | 4L -> PT_NOTE           (* 4 : note sections *)
  | 5L -> PT_SHLIB          (* 5 : reserved *)
  | 6L -> PT_PHDR           (* 6 : program header table *)

  | 0x6474e550L -> PT_GNU_EH_FRAME
  | 0x6474e551L -> PT_GNU_STACK
  | 0x6474e552L -> PT_GNU_RELRO

  | n ->
    if n >= 0x6000_0000L && n <= 0x6FFF_FFFFL then
      PT_OS n (* ( n - 0x6000_0000 ) *)
    else
      if n >= 0x7000_0000L && n <= 0x7FFF_FFFFL then
        PT_PROC n (* ( n - 0x7000_0000 ) *)
      else
        PT_NUM n

let symbol_bind_type_of_int = function
  | 0 -> STB_LOCAL
  | 1 -> STB_GLOBAL
  | 2 -> STB_WEAK
  | n ->
    if n >= 10 && n <= 12 then STB_OS (n-10) else
      if n >= 13 && n <= 15 then STB_PROC (n - 13) else
        STB_NUM n

let symbol_type_of_int = function
  | 0 -> STT_NOTYPE
  | 1 -> STT_OBJECT
  | 2 -> STT_FUNC
  | 3 -> STT_SECTION
  | 4 -> STT_FILE
  | n ->
    if n >= 10 && n <= 12 then STT_OS (n-10) else
      if n >= 13 && n <= 15 then STT_PROC (n - 13) else
        STT_NUM n

module RAW = struct

  open ElfTypes.RAW

  let section_type_of_int nL =
    match nL with
    | 0L -> SHT_NULL
    | 1L -> SHT_PROGBITS
    | 2L -> SHT_SYMTAB
    | 3L -> SHT_STRTAB
    | 4L -> SHT_RELA
    | 5L -> SHT_HASH
    | 6L -> SHT_DYNAMIC
    | 7L -> SHT_NOTE
    | 8L -> SHT_NOBITS
    | 9L -> SHT_REL
    | 10L -> SHT_SHLIB
    | 11L -> SHT_DYNSYM
    (*    | SHT_INIT_ARRAY -> "SHT_INIT_ARRAY"
          | SHT_FINI_ARRAY -> "SHT_FINI_ARRAY"
          | SHT_PREINIT_ARRAY -> "SHT_PREINIT_ARRAY"
          | SHT_GROUP -> "SHT_GROUP"
          | SHT_SYMTAB_SHNDX -> "SHT_SYMTAB_SHNDX"
    *)
    | 0x6ffffffdL -> SHT_GNU_verdef
    | 0x6ffffffeL -> SHT_GNU_verneed
    | 0x6fffffffL -> SHT_GNU_versym

    | _ ->
      if nL >= 0x60000000L && nL <= 0x6fffffffL then
        SHT_OS (Int64.to_int (Int64.sub nL 0x60000000L))
      else
        if nL >= 0x70000000L && nL <= 0x7fffffffL then
          SHT_PROC (Int64.to_int (Int64.sub nL 0x70000000L))
        else
          if nL >= 0x80000000L && nL <= 0xffffffffL then
            SHT_USER (Int64.to_int (Int64.sub nL 0x80000000L))
          else
        (*    | SHT_OS n -> Printf.sprintf "SHT_OS %d" n *)
            SHT_NUM (Int64.to_int nL)

  let get_elf_header s pos =
    let e_ident = String.sub s pos 16 in

    if int_of_char e_ident.[0] <> 0x7f ||
      e_ident.[1] <> 'E' ||
      e_ident.[2] <> 'L' ||
      e_ident.[3] <> 'F' then
      Printf.kprintf failwith "Bad magic number [%S]" e_ident;
    let e_file_class = file_class_of_int (int_of_char e_ident.[4]) in
    let e_data_encoding = data_encoding_of_int (int_of_char e_ident.[5]) in
    let e_file_version = int_of_char e_ident.[6] in
    let e_osabi = osabi_of_int (int_of_char e_ident.[7]) in
    let e_abi_version = int_of_char e_ident.[8] in
  (* other bytes are for padding *)

  (*  Printf.eprintf "e_data_encoding %s\n%!"
      (string_of_data_encoding e_data_encoding); *)
    let en = get_encoding e_data_encoding e_file_class in
    let e_type, pos = get_Elfxx_Half en s (pos+16) in
    let e_machine, pos = get_Elfxx_Half en s pos in
    let e_machine = machine_of_int e_machine in
    let e_version, pos = get_Elfxx_Word en s pos in
    let e_entry, pos = get_Elfxx_Addr en s pos in
    let e_phoff, pos = get_Elfxx_Off en s pos in
    let e_shoff, pos = get_Elfxx_Off en s pos in
    let e_flags, pos = get_Elfxx_Word en s pos in
    let e_ehsize, pos = get_Elfxx_Half en s pos in
    let e_phentsize, pos = get_Elfxx_Half en s pos in
    let e_phnum, pos = get_Elfxx_Half en s pos in
    let e_shentsize, pos = get_Elfxx_Half en s pos in
    let e_shnum, pos = get_Elfxx_Half en s pos in
    let e_shstrndx, pos = get_Elfxx_Half en s pos in
    en, {
      e_ident = e_ident;
      e_file_class = e_file_class;
      e_data_encoding = e_data_encoding;
      e_file_version = e_file_version;
      e_osabi = e_osabi;
      e_abi_version = e_abi_version;
      e_type = type_of_int e_type;
      e_machine = e_machine;
      e_version = e_version;
      e_entry = e_entry;
      e_phoff = e_phoff;
      e_shoff = e_shoff;
      e_flags = e_flags;
      e_ehsize = e_ehsize;
      e_phentsize = e_phentsize;
      e_phnum = e_phnum;
      e_shentsize = e_shentsize;
      e_shnum = e_shnum;
      e_shstrndx = e_shstrndx;
    }, pos

  let get_section_header en s pos section_size section_num =
    (*  Printf.eprintf "get_section_header %d + %d * %d\n%!"
        pos section_size section_num; *)
    let pos = pos + section_size * section_num in
    let sh_name, pos = get_Elfxx_Word en s pos in
    let sh_type, pos = get_Elfxx_Word en s pos in
    let sh_flags, pos = get_word_by_class en s pos in
    let sh_addr, pos = get_Elfxx_Addr en s pos in
    let sh_offset, pos = get_Elfxx_Off en s pos in
    let sh_size, pos = get_word_by_class en s pos in
    let sh_link, pos = get_Elfxx_Word en s pos in
    let sh_info, pos = get_Elfxx_Word en s pos in
    let sh_addralign, pos = get_word_by_class en s pos in
    let sh_entsize, pos = get_word_by_class en s pos in

    if !debug then begin
      Printf.eprintf "----------------------------------------\n";
      Printf.eprintf "sh_name %Ld\n" sh_name;
      Printf.eprintf "sh_type %Ld\n" sh_type;
      Printf.eprintf "sh_flags %Ld\n" sh_flags;
      Printf.eprintf "sh_addr %Ld\n" sh_addr;
      Printf.eprintf "sh_offset %Ld\n" sh_offset;
      Printf.eprintf "sh_size %Ld\n" sh_size;
      Printf.eprintf "sh_link %Ld\n" sh_link;
      Printf.eprintf "sh_info %Ld\n" sh_info;
      Printf.eprintf "sh_addralign %Ld\n" sh_addralign;
      Printf.eprintf "sh_entsize %Ld\n" sh_entsize;
    end;

    let sh_type = section_type_of_int sh_type in
    let sh_flags = section_header_flags_of_int en sh_flags in
    {
      sh_name = sh_name;
      sh_type = sh_type;
      sh_flags = sh_flags;
      sh_addr = sh_addr;
      sh_offset = sh_offset;
      sh_size = sh_size;
      sh_link = sh_link;
      sh_info = sh_info;
      sh_addralign = sh_addralign;
      sh_entsize;
    }

  let get_string strtab pos =
    let pos = Int64.to_int pos in
    try
      let rec iter strtab pos0 pos =
        if strtab.[pos] = '\000' then String.sub strtab pos0 (pos-pos0)
        else iter strtab pos0 (pos+1)
      in
      iter strtab pos pos
    with e ->
      Printf.eprintf "Error: get_string[%d] in strtab[%d]\n%!"
        pos (String.length strtab);
      raise e

  let get_section en string_table elf_sections_contents i sh =
    let s = elf_sections_contents.(i) in
    let section_content = s in
    let section_name = get_string string_table sh.sh_name in
    if !debug then
      Printf.eprintf "section_name = %S\n%!" section_name;
    {
      section_name;
      section_header = sh;
      section_content;
    }


  let get_program en s pos program_size program_num =
    let pos = pos + program_size * program_num in
    let program_header, pos =
      if en.word_size = ARCH32 then
        let p_type, pos = get_Elf32_Word en s pos in
        let p_offset, pos = get_Elf32_Off en s pos in
        let p_vaddr, pos = get_Elf32_Addr en s pos in
        let p_paddr, pos = get_Elf32_Addr en s pos in
        let p_filesz, pos = get_Elf32_Word en s pos in
        let p_memsz, pos = get_Elf32_Word en s pos in
        let p_flags, pos = get_Elf32_Word en s pos in
        let p_align, pos = get_Elf32_Word en s pos in

      (*
        Printf.eprintf "----------------------------------------\n";
        Printf.eprintf "p_type %Lx\n" p_type;
        Printf.eprintf "p_offset %Ld\n" p_offset;
        Printf.eprintf "p_vaddr %Ld\n" p_vaddr;
        Printf.eprintf "p_paddr %Ld\n" p_paddr;
        Printf.eprintf "p_filesz %Ld\n" p_filesz;
        Printf.eprintf "p_memsz %Ld\n" p_memsz;
        Printf.eprintf "p_flags %Ld\n" p_flags;
        Printf.eprintf "p_align %Ld\n" p_align;
      *)

        let p_type = segment_type_of_int p_type in
        {
          p_type; p_offset; p_vaddr; p_paddr; p_filesz; p_memsz; p_flags; p_align
        }, pos
      else
        let p_type, pos = get_Elf64_Word en s pos in
        let p_flags, pos = get_Elf64_Word en s pos in
        let p_offset, pos = get_Elf64_Off en s pos in
        let p_vaddr, pos = get_Elf64_Addr en s pos in
        let p_paddr, pos = get_Elf64_Addr en s pos in
        let p_filesz, pos = get_Elf64_Xword en s pos in
        let p_memsz, pos = get_Elf64_Xword en s pos in
        let p_align, pos = get_Elf64_Xword en s pos in

      (*
        Printf.eprintf "----------------------------------------\n";
        Printf.eprintf "p_type %Lx\n" p_type;
        Printf.eprintf "p_offset %Ld\n" p_offset;
        Printf.eprintf "p_vaddr %Ld\n" p_vaddr;
        Printf.eprintf "p_paddr %Ld\n" p_paddr;
        Printf.eprintf "p_filesz %Ld\n" p_filesz;
        Printf.eprintf "p_memsz %Ld\n" p_memsz;
        Printf.eprintf "p_flags %Ld\n" p_flags;
        Printf.eprintf "p_align %Ld\n" p_align;
      *)
        let p_type = segment_type_of_int p_type in
        {
          p_type; p_offset; p_vaddr; p_paddr; p_filesz; p_memsz; p_flags; p_align
        }, pos
    in
    let program_content =
      if program_header.p_filesz = 0L then "" else begin
        let p_offset = Int64.to_int program_header.p_offset in
        let p_filesz = Int64.to_int program_header.p_filesz in
      (*      Printf.eprintf "sub [%d] %d/%d\n%!" (String.length s)
              p_offset p_filesz; *)
        String.sub s p_offset p_filesz
      end
    in
    {
      program_content; program_header;
    }

  let elf_of_file_content s =
    let en, elf_header, pos = get_elf_header s 0 in
    if Int64.of_int pos <> elf_header.e_ehsize then begin
      Printf.eprintf "Warning: header size %d <> expected size %Ld\n%!"
        pos elf_header.e_ehsize;
    end;

  (* read sections *)

    let elf_section_header_table =
      if elf_header.e_shoff = 0L then [||] else
        let pos = Int64.to_int elf_header.e_shoff in
        let nsections = Int64.to_int elf_header.e_shnum in
        let section_size = Int64.to_int elf_header.e_shentsize in
        Array.init nsections (get_section_header en s pos section_size
        )
    in
    let elf_sections_contents = Array.map (fun sh ->
      if sh.sh_offset <> 0L && sh.sh_type <> SHT_NOBITS then begin
      (*      Printf.eprintf "section at %Ld len %Ld\n%!" sh.sh_offset sh.sh_size; *)
        String.sub s (Int64.to_int sh.sh_offset) (Int64.to_int sh.sh_size)
      end else ""
    ) elf_section_header_table in

    let string_table_index = Int64.to_int elf_header.e_shstrndx in
    let string_table =  elf_sections_contents.(string_table_index) in
    let elf_sections = Array.mapi
      (get_section en string_table elf_sections_contents)
      elf_section_header_table
    in

  (* read segments *)

    let elf_programs =
      if elf_header.e_phoff = 0L then [||] else
        let pos = Int64.to_int elf_header.e_phoff in
        let nprograms = Int64.to_int elf_header.e_phnum in
        let program_size = Int64.to_int elf_header.e_phentsize in
        Array.init nprograms (get_program en s pos program_size
        )
    in

    {
      elf_content = s ;
      elf_header = elf_header;
      elf_programs = elf_programs;
      elf_sections = elf_sections;
    }

  let get_verdaux en s pos =
    let vda_name, pos = get_Elfxx_Word en s pos in
    let vda_next, pos = get_Elfxx_Word en s pos in
    { vda_name; vda_next }, pos

  let get_verdef en s pos =
    let vd_version, pos = get_Elfxx_Half en s pos in
    let vd_flags, pos = get_Elfxx_Half en s pos in
    let vd_ndx, pos = get_Elfxx_Half en s pos in
    let vd_cnt, pos = get_Elfxx_Half en s pos in
    let vd_hash, pos = get_Elfxx_Word en s pos in
    let vd_aux, pos = get_Elfxx_Word en s pos in
    let vd_next, pos = get_Elfxx_Word en s pos in
    {
      vd_version; vd_flags; vd_ndx; vd_cnt;
      vd_hash; vd_aux; vd_next;
    }, pos

  let str_of_int64 strtab i = ES(i, strtab)

  let get_verneed strtab en s pos =
    let vn_version, pos = get_Elfxx_Half en s pos in
    let vn_cnt, pos = get_Elfxx_Half en s pos in
    let vn_file, pos = get_Elfxx_Word en s pos in
    let vn_aux, pos = get_Elfxx_Word en s pos in
    let vn_next, pos = get_Elfxx_Word en s pos in
    let vn_file = str_of_int64 strtab vn_file in
    { vn_version; vn_cnt; vn_file; vn_aux; vn_next }, pos

  let get_vernaux strtab en s pos =
    let vna_hash , pos = get_Elfxx_Word en s pos in
    let vna_flags , pos = get_Elfxx_Half en s pos in
    let vna_other , pos = get_Elfxx_Half en s pos in
    let vna_name , pos = get_Elfxx_Word en s pos in
    let vna_next , pos = get_Elfxx_Word en s pos in
    let vna_name = str_of_int64 strtab vna_name in
    { vna_hash; vna_flags; vna_other; vna_name; vna_next }, pos

  let get_verneed_section strtab en c pos =

    let rec get_vernauxs c pos vnas =
      let vna, _ = get_vernaux strtab en c pos in
      let vna_next = pos + Int64.to_int vna.vna_next in
      let vnas = vna :: vnas in
      if vna_next > pos then begin
        get_vernauxs c vna_next vnas
      end else List.rev vnas
    in

    let rec get_verneeds c pos vns =
      let vn, _ = get_verneed strtab en c pos in
      let vn_next = pos + Int64.to_int vn.vn_next in
      let vn_aux = pos + Int64.to_int vn.vn_aux in
      Printf.eprintf "get_verneeds %d -> vn_next = %d, vn_aux =%d\n%!" pos
        vn_next vn_aux;
      let vnas = if vn_aux > pos then get_vernauxs c vn_aux []
        else []
      in
      let vns = (vn, vnas) :: vns in
      if vn_next > pos then
        get_verneeds c vn_next vns
      else List.rev vns

    in
    get_verneeds c 0 []

  let read filename =

  (* string_of_binfile *)
    let ic = open_in_bin filename in
    let len = in_channel_length ic in
    let s = Bytes.create len in
    really_input ic s 0 len;
    close_in ic;

    elf_of_file_content (Bytes.to_string s)


  let encoding r =
    get_encoding r.elf_header.e_data_encoding r.elf_header.e_file_class


end

module ABSTRACT = struct

  module R = ElfTypes.RAW
  open ElfTypes.ABSTRACT

  let get_section r i =
    r.R.elf_sections.(Int64.to_int i).R.section_name

  let get_symbol_table_entry r string_table en s pos =
    if !debug then
      Printf.eprintf "get_symbol_table_entry[%d]\n%!" pos;
    if en.word_size = ARCH32 then
      let st_name, pos = get_Elf32_Word en s pos in
      let st_name = RAW.get_string string_table st_name in
      let st_value, pos = get_Elf32_Addr en s pos in
      let st_size, pos = get_Elf32_Word en s pos in
      let st_info, pos = get_uchar en s pos in
      let st_other, pos = get_uchar en s pos in
      let st_shndx, pos = get_Elf32_Half en s pos in
      let st_section =
        if st_shndx = shn_undefL then SYM_SHN_UNDEF
        else if st_shndx = shn_absL then SYM_SHN_ABS
        else if st_shndx = shn_commonL then SYM_SHN_COMMON
        else SYM_SHN (get_section r st_shndx)
      in
      let st_bind =
        let st_info = Int64.to_int st_info in
        symbol_bind_type_of_int ( (st_info lsr 4) land 0xf ) in
      let st_type =
        let st_info = Int64.to_int st_info in
        symbol_type_of_int ( st_info land 0xf ) in
      {
        st_name = st_name;
        st_value = st_value;
        st_size = st_size;
        st_bind = st_bind;
        st_type = st_type;
        (*        st_info = st_info; *)
        (*        st_other = st_other; *)
        st_section = st_section;
      }
    else
      let st_name, pos = get_Elf64_Word en s pos in
      let st_name2 = RAW.get_string string_table st_name in
      if !debug then
        Printf.eprintf "read symbol %Ld -> %S\n%!" st_name st_name2;
      let st_name = st_name2 in

      let st_info, pos = get_uchar en s pos in
      let st_other, pos = get_uchar en s pos in
      let st_shndx, pos = get_Elf64_Half en s pos in
      let st_section =
        if st_shndx = shn_undefL then SYM_SHN_UNDEF
        else if st_shndx = shn_absL then SYM_SHN_ABS
        else if st_shndx = shn_commonL then SYM_SHN_COMMON
        else SYM_SHN (get_section r st_shndx)
      in
      let st_value, pos = get_Elf64_Addr en s pos in
      let st_size, pos = get_Elf64_Xword en s pos in
      let st_bind =
        let st_info = Int64.to_int st_info in
        symbol_bind_type_of_int ( (st_info lsr 4) land 0xf ) in
      let st_type =
        let st_info = Int64.to_int st_info in
        symbol_type_of_int ( st_info land 0xf ) in

      {
        st_name = st_name;
        st_value = st_value;
        st_size = st_size;
        (*        st_info = st_info; *)
        st_bind = st_bind;
        st_type = st_type;
        (*        st_other = st_other; *)
        st_section = st_section;
      }


  let rtype64_of_int r_type =
    match Int64.to_int r_type with
    |   0 -> R_X86_64_NONE
    |   1 -> R_X86_64_64
    |   2 -> R_X86_64_PC32
    |   3 -> R_X86_64_GOT32
    |   4 -> R_X86_64_PLT32
    |   5 -> R_X86_64_COPY
    |   6 -> R_X86_64_GLOB_DAT
    |   7 -> R_X86_64_JUMP_SLOT
    |   8 -> R_X86_64_RELATIVE
    |   9 -> R_X86_64_GOTPCREL
    |  10 -> R_X86_64_32
    |  11 -> R_X86_64_32S
    |  12 -> R_X86_64_16
    |  13 -> R_X86_64_PC16
    |  14 -> R_X86_64_8
    |  15 -> R_X86_64_PC8
    |  16 -> R_X86_64_DPTMOD64
    |  17 -> R_X86_64_DTPOFF64
    |  18 -> R_X86_64_TPOFF64
    |  19 -> R_X86_64_TLSGD
    |  20 -> R_X86_64_TLSLD
    |  21 -> R_X86_64_DTPOFF32
    |  22 -> R_X86_64_GOTTPOFF
    |  23 -> R_X86_64_TPOFF32
    |  24 -> R_X86_64_PC64
    |  25 -> R_X86_64_GOTOFF64
    |  26 -> R_X86_64_GOTPC32
    |  32 -> R_X86_64_SIZE32
    |  33 -> R_X86_64_SIZE64
    | n -> R_X86_64_UNKNOWN n

  let rtype32_of_int r_type =
    match Int64.to_int r_type with
    | 0 -> R_386_NONE
    | 1 -> R_386_32
    | 2 -> R_386_PC32
    | 3 -> R_386_GOT32
    | 4 -> R_386_PLT32
    | 5 -> R_386_COPY
    | 6 -> R_386_GLOB_DAT
    | 7 -> R_386_JMP_SLOT
    | 8 -> R_386_RELATIVE
    | 9 -> R_386_GOTOFF
    | 10 -> R_386_GOTPC
    | 11 -> R_386_32PLT
    | 20 -> R_386_16
    | 21 -> R_386_PC16
    | 22 -> R_386_8
    | 23 -> R_386_PC8
    | n -> R_386_UNKNOWN n


  let get_rel64 get_addend sh section_name en c =
    let reloc_size = Int64.to_int sh.R.sh_entsize in
    let nrelocs = Int64.to_int sh.R.sh_size / reloc_size in
    let rela_symbols = section_name sh.R.sh_link in
    let rela_target = section_name sh.R.sh_info in
    let rela_relocs = Array.init nrelocs (fun i ->
      let pos = i * reloc_size in
      let r_offset, pos = get_Elf64_Addr en c pos in
      let rela_info, pos = get_Elf64_Xword en c pos in
      let r_addend, pos = get_addend en c pos in
      let rela_sym = Int64.shift_right rela_info 32 in
      let r_sym = Int64.logand rela_sym 0xffff_ffffL in
      let r_type = Int64.logand rela_info 0xffff_ffffL in
      let r_type = rtype64_of_int r_type in
      {
        r_offset;
        r_sym;
        r_type;
        r_addend;
      }
    ) in

    { rela_symbols; rela_relocs; rela_target }

  let get_rel32 get_addend sh section_name en c =
    let reloc_size = Int64.to_int sh.R.sh_entsize in
    let nrelocs = Int64.to_int sh.R.sh_size / reloc_size in
    let rela_symbols = section_name sh.R.sh_link in
    let rela_target = section_name sh.R.sh_info in

    let rela_relocs = Array.init nrelocs (fun i ->
      let pos = i * reloc_size in
      let r_offset, pos = get_Elf32_Addr en c pos in
      let rela_info, pos = get_Elf32_Word en c pos in
      let r_addend, pos = get_addend en c pos in

      let rela_sym = Int64.shift_right rela_info 8 in
      let r_sym = Int64.logand rela_sym 0xff_ffffL in
      let r_type = Int64.logand rela_info 0xffL in
      let r_type = rtype32_of_int r_type in
      {
        r_offset;
        r_sym;
        r_type;
        r_addend;
      }
    ) in
    { rela_symbols; rela_relocs; rela_target }


  let of_raw r =
    let h = r.R.elf_header in
    let e = {
          (*      e_ident = h.R.e_ident; *)
      e_file_class = h.R.e_file_class;
      e_data_encoding = h.R.e_data_encoding;
      e_file_version = h.R.e_file_version;
      e_osabi = h.R.e_osabi;
      e_abi_version = h.R.e_abi_version;
      e_type = h.R.e_type;
      e_machine = h.R.e_machine;
      e_version = h.R.e_version;
      e_entry = h.R.e_entry;
      e_flags = h.R.e_flags;
      e_sections = StringMap.empty;
      e_programs = [||];
    } in

    let en = RAW.encoding r in
    let section_content n =
      r.R.elf_sections.(Int64.to_int n).R.section_content
    in
    let section_name n =
      r.R.elf_sections.(Int64.to_int n).R.section_name
    in
    let parse_rel sh c =
      match sh.R.sh_entsize with
      | 24L -> SHT_RELA64 (get_rel64 get_Elf64_Sxword sh section_name en c )
      | 16L ->
        SHT_REL64 (get_rel64 (fun en _ pos -> (0L, pos)) sh section_name en c )
      | 12L -> SHT_RELA32 (get_rel32 get_Elf32_Sword sh section_name en c )
      | 8L ->
        SHT_REL32 (get_rel32 (fun en _ pos -> (0L, pos)) sh section_name en c )
      | _ ->
        Printf.kprintf failwith "Unexpected REL/RELA size %Ld" sh.R.sh_entsize
    in
    let string_table_index = h.R.e_shstrndx in
    let string_table =  section_content string_table_index in
    Array.iteri (fun i s ->
      let s_name = s.R.section_name in
      let c = s.R.section_content in
      let sh = s.R.section_header in
      let prog_bits sh c =
        {
          sht_flags = sh.R.sh_flags;
          sht_addr = sh.R.sh_addr;
              (*          sht_size = Int64.to_int sh.R.sh_size; *)
          sht_align = Int64.to_int sh.R.sh_addralign;
          sht_content = c;
        } in
      let s_desc = match sh.R.sh_type with
        | R.SHT_NOBITS -> SHT_NOBITS (prog_bits sh c)
        | R.SHT_PROGBITS -> SHT_PROGBITS (prog_bits sh c)
        | R.SHT_STRTAB -> SHT_STRTAB s.R.section_content
        | R.SHT_SYMTAB ->
          let string_table =
            if sh.R.sh_link = 0L then string_table else
              section_content sh.R.sh_link
          in
          let symbol_entry_size = Int64.to_int sh.R.sh_entsize in
          let nsymbols =
            if symbol_entry_size = 0 then 0 else
              Int64.to_int sh.R.sh_size / symbol_entry_size
          in
          let symbols = Array.init nsymbols (fun i ->
            get_symbol_table_entry r string_table en c
              (i * symbol_entry_size)
          ) in
          SHT_SYMTAB symbols
        | R.SHT_NULL -> SHT_NULL
        | R.SHT_REL
        | R.SHT_RELA
          -> parse_rel sh c
        | R.SHT_HASH
        | R.SHT_DYNAMIC
        | R.SHT_NOTE
        | R.SHT_SHLIB
        | R.SHT_DYNSYM
        | R.SHT_INIT_ARRAY
        | R.SHT_FINI_ARRAY
        | R.SHT_PREINIT_ARRAY
        | R.SHT_GROUP
        | R.SHT_SYMTAB_SHNDX
        | R.SHT_NUM _
        | R.SHT_OS _
        | R.SHT_PROC _
        | R.SHT_USER _

        | R.SHT_GNU_verdef
          -> SHT_UNKNOWN s

        | R.SHT_GNU_verneed ->
          let strtab = section_content sh.R.sh_link in
          let vns = RAW.get_verneed_section strtab en c 0 in
          SHT_VERNEED (section_name sh.R.sh_link, vns)

        | R.SHT_GNU_versym ->
          let size = String.length c / 4 in
          let versions = Array.init size (fun i ->
            let n, _ = get_Elfxx_Half en c (4*i) in Int64.to_int n
          ) in
          SHT_VERSYM versions
      in

      let ss = { s_name; s_desc } in
      e.e_sections <- StringMap.add s_name ss e.e_sections
    ) r.R.elf_sections;

    e.e_programs <-
      Array.mapi (fun i p ->
        let ph = p.R.program_header in
        let c = p.R.program_content in
        let p_desc =
          match ph.R.p_type with
          | PT_INTERP ->
            PT_INTERP (get_string0 c 0)
          | _ ->
            PT_UNKNOWN
        in
        { p_header = ph; p_desc }
      ) r.R.elf_programs;

    e

  let read filename =
    of_raw (RAW.read filename)

end
