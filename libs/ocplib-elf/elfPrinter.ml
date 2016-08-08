(**************************************************************************)
(*                                                                        *)
(*  Copyright 2012 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

open StringCompat
open ElfTypes

let strtab = ref true
let compact = ref true

type indent = string

  let string_of_type = function
    | ET_NONE -> "ET_NONE"
    | ET_REL -> "ET_REL"
    | ET_EXEC -> "ET_EXEC"
    | ET_DYN -> "ET_DYN"
    | ET_CORE -> "ET_CORE"
    | ET_NUM n -> Printf.sprintf "ET_NUM %d" n
    | ET_OS n -> Printf.sprintf "ET_OS %d" n
    | ET_PROC n -> Printf.sprintf "ET_PROC %d" n

  let string_of_file_class = function
    | ELFCLASSNONE -> "ELFCLASSNONE"
    | ELFCLASS32 -> "ELFCLASS32"
    | ELFCLASS64 -> "ELFCLASS64"
    | ELFCLASSNUM n -> Printf.sprintf "ELFCLASSNUM %d" n


  let string_of_data_encoding = function
    | ELFDATANONE -> "ELFDATANONE"
    | ELFDATA2LSB -> "ELFDATA2LSB"
    | ELFDATA2MSB -> "ELFDATA2MSB"
    | ELFDATANUM n -> Printf.sprintf "ELFDATANUM %d" n

  let string_of_osabi = function
    | ELFOSABI_NONE -> "ELFOSABI_NONE"
    | ELFOSABI_SYSV -> "ELFOSABI_SYSV"
    | ELFOSABI_HPUX -> "ELFOSABI_HPUX"
    | ELFOSABI_NETBSD -> "ELFOSABI_NETBSD"
    | ELFOSABI_LINUX -> "ELFOSABI_LINUX"
    | ELFOSABI_SOLARIS -> "ELFOSABI_SOLARIS"
    | ELFOSABI_AIX -> "ELFOSABI_AIX"
    | ELFOSABI_IRIX -> "ELFOSABI_IRIX"
    | ELFOSABI_FREEBSD -> "ELFOSABI_FREEBSD"
    | ELFOSABI_TRU64 -> "ELFOSABI_TRU64"
    | ELFOSABI_MODESTO -> "ELFOSABI_MODESTO"
    | ELFOSABI_OPENBSD -> "ELFOSABI_OPENBSD"
    | ELFOSABI_OPENVMS -> "ELFOSABI_OPENVMS"
    | ELFOSABI_NSK -> "ELFOSABI_NSK"
    | ELFOSABI_AROS -> "ELFOSABI_AROS"
    | ELFOSABI_ARM -> "ELFOSABI_ARM"
    | ELFOSABI_STANDALONE -> "ELFOSABI_STANDALONE"
    | ELFOSABI_NUM n -> Printf.sprintf "ELFOSABI %d" n

  let string_of_machine = function
    | EM_NONE        -> "EM_NONE"
    | EM_M32         -> "EM_M32"
    | EM_SPARC       -> "EM_SPARC"
    | EM_386         -> "EM_386"
    | EM_68K         -> "EM_68K"
    | EM_88K         -> "EM_88K"
    | EM_486         -> "EM_486"
    | EM_860         -> "EM_860"
    | EM_MIPS        -> "EM_MIPS"
    | EM_S370        -> "EM_S370"
    | EM_MIPS_RS3_LE -> "EM_MIPS_RS3_LE"
    | EM_SPARC64     -> "EM_SPARC64"
    | EM_PARISC      -> "EM_PARISC"
    | EM_VPP500      -> "EM_VPP500"
    | EM_SPARC32PLUS -> "EM_SPARC32PLUS"
    | EM_960         -> "EM_960"
    | EM_PPC         -> "EM_PPC"
    | EM_PPC64       -> "EM_PPC64"
    | EM_S390        -> "EM_S390"
    | EM_SPU         -> "EM_SPU"
    | EM_V800        -> "EM_V800"
    | EM_FR20        -> "EM_FR20"
    | EM_RH32        -> "EM_RH32"
    | EM_RCE         -> "EM_RCE"
    | EM_ARM         -> "EM_ARM"
    | EM_ALPHA       -> "EM_ALPHA"
    | EM_SH          -> "EM_SH"
    | EM_SPARCV9     -> "EM_SPARCV9"
    | EM_TRICORE     -> "EM_TRICORE"
    | EM_ARC         -> "EM_ARC"
    | EM_H8_300      -> "EM_H8_300"
    | EM_H8_300H     -> "EM_H8_300H"
    | EM_H8S         -> "EM_H8S"
    | EM_H8_500      -> "EM_H8_500"
    | EM_IA_64       -> "EM_IA_64"
    | EM_MIPS_X      -> "EM_MIPS_X"
    | EM_COLDFIRE    -> "EM_COLDFIRE"
    | EM_68HC12      -> "EM_68HC12"
    | EM_MMA         -> "EM_MMA"
    | EM_PCP         -> "EM_PCP"
    | EM_NCPU        -> "EM_NCPU"
    | EM_NDR1        -> "EM_NDR1"
    | EM_STARCORE    -> "EM_STARCORE"
    | EM_ME16        -> "EM_ME16"
    | EM_ST100       -> "EM_ST100"
    | EM_TINYJ       -> "EM_TINYJ"
    | EM_X86_64      -> "EM_X86_64"
    | EM_PDSP        -> "EM_PDSP"
    | EM_FX66        -> "EM_FX66"
    | EM_ST9PLUS     -> "EM_ST9PLUS"
    | EM_ST7         -> "EM_ST7"
    | EM_68HC16      -> "EM_68HC16"
    | EM_68HC11      -> "EM_68HC11"
    | EM_68HC08      -> "EM_68HC08"
    | EM_68HC05      -> "EM_68HC05"
    | EM_SVX         -> "EM_SVX"
    | EM_ST19        -> "EM_ST19"
    | EM_VAX         -> "EM_VAX"
    | EM_CRIS        -> "EM_CRIS"
    | EM_JAVELIN     -> "EM_JAVELIN"
    | EM_FIREPATH    -> "EM_FIREPATH"
    | EM_ZSP         -> "EM_ZSP"
    | EM_MMIX        -> "EM_MMIX"
    | EM_HUANY       -> "EM_HUANY"
    | EM_PRISM       -> "EM_PRISM"
    | EM_AVR         -> "EM_AVR"
    | EM_FR30        -> "EM_FR30"
    | EM_D10V        -> "EM_D10V"
    | EM_D30V        -> "EM_D30V"
    | EM_V850        -> "EM_V850"
    | EM_M32R        -> "EM_M32R"
    | EM_MN10300     -> "EM_MN10300"
    | EM_MN10200     -> "EM_MN10200"
    | EM_PJ          -> "EM_PJ"
    | EM_OPENRISC    -> "EM_OPENRISC"
    | EM_ARC_A5      -> "EM_ARC_A5"
    | EM_XTENSA      -> "EM_XTENSA"
    | EM_VIDEOCORE   -> "EM_VIDEOCORE"
    | EM_TMM_GPP     -> "EM_TMM_GPP"
    | EM_NS32K       -> "EM_NS32K"
    | EM_TPC         -> "EM_TPC"
    | EM_SNP1K       -> "EM_SNP1K"
    | EM_ST200       -> "EM_ST200"
    | EM_IP2K        -> "EM_IP2K"
    | EM_MAX         -> "EM_MAX"
    | EM_CR          -> "EM_CR"
    | EM_F2MC16      -> "EM_F2MC16"
    | EM_MSP430      -> "EM_MSP430"
    | EM_BLACKFIN    -> "EM_BLACKFIN"
    | EM_SE_C33      -> "EM_SE_C33"
    | EM_SEP         -> "EM_SEP"
    | EM_ARCA        -> "EM_ARCA"
    | EM_UNICORE     -> "EM_UNICORE"
    | EM_NUM n -> Printf.sprintf "EM_NUM %d" n

  let info_of_machine = function
    | EM_NONE        -> "No machine"
    | EM_M32         -> "AT&T WE 32100"
    | EM_SPARC       -> "SPARC"
    | EM_386         -> "Intel 80386"
    | EM_68K         -> "Motorola 68000"
    | EM_88K         -> "Motorola 88000"
    | EM_486         -> "Intel i486 (DO NOT USE THIS ONE)"
    | EM_860         -> "Intel 80860"
    | EM_MIPS        -> "MIPS I Architecture"
    | EM_S370        -> "IBM System/370 Processor"
    | EM_MIPS_RS3_LE -> "MIPS RS3000 Little-endian"
    | EM_SPARC64     -> "SPARC 64-bit"
    | EM_PARISC      -> "Hewlett-Packard PA-RISC"
    | EM_VPP500      -> "Fujitsu VPP500"
    | EM_SPARC32PLUS -> "Enhanced instruction set SPARC"
    | EM_960         -> "Intel 80960"
    | EM_PPC         -> "PowerPC"
    | EM_PPC64       -> "64-bit PowerPC"
    | EM_S390        -> "IBM System/390 Processor"
    | EM_SPU         -> "Cell SPU"
    | EM_V800        -> "NEC V800"
    | EM_FR20        -> "Fujitsu FR20"
    | EM_RH32        -> "TRW RH-32"
    | EM_RCE         -> "Motorola RCE"
    | EM_ARM         -> "Advanced RISC Machines ARM"
    | EM_ALPHA       -> "Digital Alpha"
    | EM_SH          -> "Hitachi SH"
    | EM_SPARCV9     -> "SPARC Version 9"
    | EM_TRICORE     -> "Siemens TriCore embedded processor"
    | EM_ARC         -> "Argonaut RISC Core, Argonaut Technologies Inc."
    | EM_H8_300      -> "Hitachi H8/300"
    | EM_H8_300H     -> "Hitachi H8/300H"
    | EM_H8S         -> "Hitachi H8S"
    | EM_H8_500      -> "Hitachi H8/500"
    | EM_IA_64       -> "Intel IA-64 processor architecture"
    | EM_MIPS_X      -> "Stanford MIPS-X"
    | EM_COLDFIRE    -> "Motorola ColdFire"
    | EM_68HC12      -> "Motorola M68HC12"
    | EM_MMA         -> "Fujitsu MMA Multimedia Accelerator"
    | EM_PCP         -> "Siemens PCP"
    | EM_NCPU        -> "Sony nCPU embedded RISC processor"
    | EM_NDR1        -> "Denso NDR1 microprocessor"
    | EM_STARCORE    -> "Motorola Star*Core processor"
    | EM_ME16        -> "Toyota ME16 processor"
    | EM_ST100       -> "STMicroelectronics ST100 processor"
    | EM_TINYJ       -> "Advanced Logic Corp. TinyJ embedded processor family"
    | EM_X86_64      -> "AMD x86-64 architecture"
    | EM_PDSP        -> "Sony DSP Processor"
    | EM_FX66        -> "Siemens FX66 microcontroller"
    | EM_ST9PLUS     -> "STMicroelectronics ST9+ 8/16 bit microcontroller"
    | EM_ST7         -> "STMicroelectronics ST7 8-bit microcontroller"
    | EM_68HC16      -> "Motorola MC68HC16 Microcontroller"
    | EM_68HC11      -> "Motorola MC68HC11 Microcontroller"
    | EM_68HC08      -> "Motorola MC68HC08 Microcontroller"
    | EM_68HC05      -> "Motorola MC68HC05 Microcontroller"
    | EM_SVX         -> "Silicon Graphics SVx"
    | EM_ST19        -> "STMicroelectronics ST19 8-bit microcontroller"
    | EM_VAX         -> "Digital VAX"
    | EM_CRIS        -> "Axis Communications 32-bit embedded processor"
    | EM_JAVELIN     -> "Infineon Technologies 32-bit embedded processor"
    | EM_FIREPATH    -> "Element 14 64-bit DSP Processor"
    | EM_ZSP         -> "LSI Logic 16-bit DSP Processor"
    | EM_MMIX        -> "Donald Knuth's educational 64-bit processor"
    | EM_HUANY       -> "Harvard University machine-independent object files"
    | EM_PRISM       -> "SiTera Prism"
    | EM_AVR         -> "Atmel AVR 8-bit microcontroller"
    | EM_FR30        -> "Fujitsu FR30"
    | EM_D10V        -> "Mitsubishi D10V"
    | EM_D30V        -> "Mitsubishi D30V"
    | EM_V850        -> "NEC v850"
    | EM_M32R        -> "Mitsubishi M32R"
    | EM_MN10300     -> "Matsushita MN10300"
    | EM_MN10200     -> "Matsushita MN10200"
    | EM_PJ          -> "picoJava"
    | EM_OPENRISC    -> "OpenRISC 32-bit embedded processor"
    | EM_ARC_A5      -> "ARC Cores Tangent-A5"
    | EM_XTENSA      -> "Tensilica Xtensa Architecture"
    | EM_VIDEOCORE   -> "Alphamosaic VideoCore processor"
    | EM_TMM_GPP     -> "Thompson Multimedia General Purpose Processor"
    | EM_NS32K       -> "National Semiconductor 32000 series"
    | EM_TPC         -> "Tenor Network TPC processor"
    | EM_SNP1K       -> "Trebia SNP 1000 processor"
    | EM_ST200       -> "STMicroelectronics (www.st.com) ST200 microcontroller"
    | EM_IP2K        -> "Ubicom IP2xxx microcontroller family"
    | EM_MAX         -> "MAX Processor"
    | EM_CR          -> "National Semiconductor CompactRISC microprocessor"
    | EM_F2MC16      -> "Fujitsu F2MC16"
    | EM_MSP430      -> "Texas Instruments embedded microcontroller msp430"
    | EM_BLACKFIN    -> "Analog Devices Blackfin (DSP) processor"
    | EM_SE_C33      -> "S1C33 Family of Seiko Epson processors"
    | EM_SEP         -> "Sharp embedded microprocessor"
    | EM_ARCA        -> "Arca RISC Microprocessor"
    | EM_UNICORE     -> "Microprocessor series from PKU-Unity Ltd. and MPRC of Peking University"
    | EM_NUM n -> Printf.sprintf "unknown EM_NUM %d" n

  let string_of_section_header_flag = function
    | SHF_WRITE -> "SHF_WRITE"
    | SHF_ALLOC ->   "SHF_ALLOC"
    | SHF_EXECINSTR -> "SHF_EXECINSTR"
    | SHF_NUM n -> Printf.sprintf "SHF_NUM %d" n

  let string_of_symbol_bind_type = function
    | STB_LOCAL -> "STB_LOCAL"
    | STB_GLOBAL -> "STB_GLOBAL"
    | STB_WEAK -> "STB_WEAK"
    | STB_OS n -> Printf.sprintf "STB_OS %d" n
    | STB_PROC n -> Printf.sprintf "STB_PROC %d" n
    | STB_NUM n -> Printf.sprintf "STB_NUM %d" n

  let string_of_symbol_type = function
    | STT_NOTYPE -> "STT_NOTYPE"
    | STT_OBJECT -> "STT_OBJECT"
    | STT_FUNC -> "STT_FUNC"
    | STT_SECTION -> "STT_SECTION"
    | STT_FILE -> "STT_FILE"
    | STT_OS n -> Printf.sprintf "STT_OS %d" n
    | STT_PROC n -> Printf.sprintf "STT_PROC %d" n
    | STT_NUM n -> Printf.sprintf "STT_NUM %d" n

let segment_type = function
  | PT_NULL -> "PT_NULL"
  | PT_LOAD -> "PT_LOAD"
  | PT_DYNAMIC -> "PT_DYNAMIC"
  | PT_INTERP -> "PT_INTERP"
  | PT_NOTE -> "PT_NOTE"
  | PT_SHLIB -> "PT_SHLIB"
  | PT_PHDR -> "PT_PHDR"
  | PT_OS n -> Printf.sprintf "PT_OS 0x%Lx" n
  | PT_PROC n -> Printf.sprintf "PT_PROC 0x%Lx" n
  | PT_NUM n -> Printf.sprintf "PT_NUM 0x%Lx" n

    | PT_GNU_EH_FRAME -> "PT_GNU_EH_FRAME"
    | PT_GNU_STACK -> "PT_GNU_STACK"
    | PT_GNU_RELRO -> "PT_GNU_RELRO"

let string_of_addr v =
  Printf.sprintf "0x%LxL (* %Ld *)" v v

module RAW = struct
  open ElfTypes.RAW


  let string_of_section_type = function
    | SHT_NULL -> "SHT_NULL"
    | SHT_PROGBITS -> "SHT_PROGBITS"
    | SHT_SYMTAB -> "SHT_SYMTAB"
    | SHT_STRTAB -> "SHT_STRTAB"
    | SHT_RELA -> "SHT_RELA"
    | SHT_HASH -> "SHT_HASH"
    | SHT_DYNAMIC -> "SHT_DYNAMIC"
    | SHT_NOTE -> "SHT_NOTE"
    | SHT_NOBITS -> "SHT_NOBITS"
    | SHT_REL -> "SHT_REL"
    | SHT_SHLIB -> "SHT_SHLIB"
    | SHT_DYNSYM -> "SHT_DYNSYM"
    | SHT_INIT_ARRAY -> "SHT_INIT_ARRAY"
    | SHT_FINI_ARRAY -> "SHT_FINI_ARRAY"
    | SHT_PREINIT_ARRAY -> "SHT_PREINIT_ARRAY"
    | SHT_GROUP -> "SHT_GROUP"
    | SHT_SYMTAB_SHNDX -> "SHT_SYMTAB_SHNDX"
    | SHT_GNU_verdef -> "SHT_GNU_verdef"
    | SHT_GNU_verneed -> "SHT_GNU_verneed"
    | SHT_GNU_versym -> "SHT_GNU_versym"

    | SHT_NUM n -> Printf.sprintf "SHT_NUM %d" n
    | SHT_OS n -> Printf.sprintf "SHT_OS %d" n
    | SHT_PROC n -> Printf.sprintf "SHT_PROC %d" n
    | SHT_USER n -> Printf.sprintf "SHT_USER %d" n

  let section_header b indent sh =
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s sh_name = %LdL\n" indent sh.sh_name;
    Printf.bprintf b "%s sh_type = %s;\n" indent (string_of_section_type sh.sh_type);
    Printf.bprintf b "%s sh_flags = [" indent;
      List.iter (fun f ->
        Printf.bprintf b " %s;" (string_of_section_header_flag f))
        sh.sh_flags;
      Printf.bprintf b "];\n";

    Printf.bprintf b "%s sh_addr = %s;\n" indent (string_of_addr sh.sh_addr);
    Printf.bprintf b "%s sh_offset = %Ld;\n" indent sh.sh_offset;
    Printf.bprintf b "%s sh_size = %Ld;\n" indent sh.sh_size;
    Printf.bprintf b "%s sh_link = %Ld;\n" indent sh.sh_link;
    Printf.bprintf b "%s sh_info = %Ld;\n" indent sh.sh_info;
    Printf.bprintf b "%s sh_addralign = %Ld;\n" indent sh.sh_addralign;
    Printf.bprintf b "%s sh_entsize = %Ld;\n" indent sh.sh_entsize;
    Printf.bprintf b "%s}" indent

  let section b indent s =
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s section_name = %S;\n" indent s.section_name;
    Printf.bprintf b "%s section_content = string[%d];\n" indent
      (String.length s.section_content);
    Printf.bprintf b "%s section_header = " indent;
    section_header b (indent ^ "  ") s.section_header;
    Printf.bprintf b ";\n";
    Printf.bprintf b "%s}" indent

  let program_header b indent p =
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s p_type = %s\n" indent (segment_type p.p_type);
    Printf.bprintf b "%s p_flags = 0x%LdL;\n" indent p.p_flags;
    Printf.bprintf b "%s p_offset = %Ld (* 0x%LxL *);\n"
      indent p.p_offset p.p_offset;
    Printf.bprintf b "%s p_vaddr = %s;\n" indent (string_of_addr p.p_vaddr);
    Printf.bprintf b "%s p_paddr = %s;\n" indent (string_of_addr p.p_paddr);
    Printf.bprintf b "%s p_filesz = %s;\n" indent (string_of_addr p.p_filesz);
    Printf.bprintf b "%s p_memsz = %Ld;\n" indent p.p_memsz;
    Printf.bprintf b "%s p_align = %Ld;\n" indent p.p_align;
    Printf.bprintf b "%s}" indent


  let program b indent p =
    Printf.bprintf b "{\n";
(*    Printf.bprintf b "%s section_name = %S;\n" indent s.section_name; *)
    Printf.bprintf b "%s program_content = string[%d];\n" indent
      (String.length p.program_content);
    Printf.bprintf b "%s program_header = " indent;
    program_header b (indent ^ "  ") p.program_header;
    Printf.bprintf b ";\n";
    Printf.bprintf b "%s}" indent

  let header b indent h =
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s  e_ident = %S; (* magic number *)\n" indent h.e_ident;
    Printf.bprintf b "%s  e_file_class = %s;\n" indent (string_of_file_class h.e_file_class);
    Printf.bprintf b "%s e_data_encoding = %s;\n" indent (string_of_data_encoding h.e_data_encoding);
    Printf.bprintf b "%s e_file_version = %d;\n" indent h.e_file_version;
    Printf.bprintf b "%s e_osabi = %s;\n" indent (string_of_osabi h.e_osabi);
    Printf.bprintf b "%s e_abi_version = %d;\n" indent h.e_abi_version;
    Printf.bprintf b "%s e_type = %s;\n" indent (string_of_type h.e_type);
    Printf.bprintf b "%s e_machine = %s;\n" indent (string_of_machine h.e_machine);
    Printf.bprintf b "%s e_version = %Ld;\n"  indent h.e_version;
    Printf.bprintf b "%s e_entry = %Ld; (* entry point addr *)\n"
      indent h.e_entry;
    Printf.bprintf b "%s e_phoff = %Ld; (*program header table offset*)\n"
      indent h.e_phoff;
    Printf.bprintf b "%s e_shoff = %Ld; (*section header table offset*)\n"
      indent h.e_shoff;
    Printf.bprintf b "%s e_flags = %Ld;\n" indent h.e_flags;
    Printf.bprintf b "%s e_ehsize = %Ld; (*elf header size*)\n" indent h.e_ehsize;
    Printf.bprintf b "%s e_phentsize = %Ld; (*program header table entry size*)\n" indent h.e_phentsize;
    Printf.bprintf b "%s e_phnum = %Ld; (*program header table number of entries*)\n" indent h.e_phnum;
    Printf.bprintf b "%s e_shentsize = %Ld; (*section header table entry size*)\n" indent h.e_shentsize;
    Printf.bprintf b "%s e_shnum = %Ld; (*section header table number of entries*)\n" indent h.e_shnum;
    Printf.bprintf b "%s e_shstrndx = %Ld; (*string table section index in section header table*)\n" indent h.e_shstrndx;
    Printf.bprintf b "%s}" indent


(*
type verdef = {
  vd_version : int64;
  vd_flags : int64;
  vd_ndx : int64;
  vd_cnt : int64;
  vd_hash : int64;
  vd_aux : int64;
  vd_next : int64;
}
type verdaux = {
  vda_name : int64;
  vda_next : int64;
}
*)

  let string_of_str = function
    | S s -> Printf.sprintf "%S" s
    | ES (pos, strtab) ->
      let pos = Int64.to_int pos in
      let s =
        try
          let pos2 = String.index_from strtab pos '\000' in
          String.sub strtab pos (pos2-pos)
        with _ ->
          Printf.eprintf "Error: string_of_str (%d, len = %d) failed\n%!"
            pos (String.length strtab);
          "?????"
      in
      Printf.sprintf "%dL (* %S *)" pos s

let verneed b indent vn =
      Printf.bprintf b "{\n";
      Printf.bprintf b "%s vn_version = %Ld;\n" indent vn.vn_version;
      Printf.bprintf b "%s vn_cnt = %Ld;\n" indent vn.vn_cnt;
      Printf.bprintf b "%s vn_file = %s;\n" indent
        (string_of_str vn.vn_file);
      Printf.bprintf b "%s vn_aux = %Ld;\n" indent vn.vn_aux;
      Printf.bprintf b "%s vn_next = %Ld;\n" indent vn.vn_next;
      Printf.bprintf b "%s}" indent

let vernaux b indent vna =
      Printf.bprintf b "{\n";
      Printf.bprintf b "%s vna_hash = %Ld;\n" indent vna.vna_hash;
      Printf.bprintf b "%s vna_flags = %Ld;\n" indent vna.vna_flags;
      Printf.bprintf b "%s vna_other = %Ld;\n" indent vna.vna_other;
      Printf.bprintf b "%s vna_name = %s;\n" indent
        (string_of_str vna.vna_name);
      Printf.bprintf b "%s vna_next = %Ld;\n" indent vna.vna_next;
      Printf.bprintf b "%s}" indent


  let to_ocaml indent elf =
    let b = Buffer.create 1000 in
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s elf_content = string[%d];\n"
      indent (String.length elf.elf_content);
    Printf.bprintf b "%s elf_header = " indent;
    header b (indent ^ "    ") elf.elf_header;
    Printf.bprintf b ";\n";

    Printf.bprintf b "%s elf_programs = [| (* %d programs *)\n"
      indent (Array.length elf.elf_programs);
    Array.iteri (fun i s ->
      Printf.bprintf b "%s   (* program %d *) " indent i;
      program b (indent ^ "     ") s;
      Printf.bprintf b ";\n";
    ) elf.elf_programs;
    Printf.bprintf b "%s  |];\n" indent;

    Printf.bprintf b "%s elf_sections = [| (* %d sections *)\n"
      indent (Array.length elf.elf_sections);
    Array.iteri (fun i s ->
      Printf.bprintf b "%s   (* section %d *) " indent i;
      section b (indent ^ "     ") s;
      Printf.bprintf b ";\n";
    ) elf.elf_sections;
    Printf.bprintf b "%s  |];\n" indent;
    Buffer.contents b

end

module ABSTRACT = struct

  open ElfTypes.ABSTRACT



  let symbol b indent st =
    if !compact then begin
      Printf.bprintf b "{";
      Printf.bprintf b " st_name = %S;" st.st_name;
      Printf.bprintf b " st_value = %s;"  (string_of_addr st.st_value);
      Printf.bprintf b " st_size = %Ld;\n"  st.st_size;
      Printf.bprintf b " st_bind = %s;"
        (string_of_symbol_bind_type st.st_bind);
      Printf.bprintf b " st_type = %s;"
        (string_of_symbol_type st.st_type);
      (*    Printf.bprintf b "%s st_other = %s;\n" indent st.st_other; *)
      Printf.bprintf b " st_section = %s; }"
        (match st.st_section with
         | SYM_SHN_UNDEF -> "SYM_SHN_UNDEF"
         | SYM_SHN_ABS -> "SYM_SHN_ABS"
         | SYM_SHN_COMMON -> "SYM_SHN_COMMON"
         | SYM_SHN s -> Printf.sprintf "SYM_SHN %S" s
        );
    end  else  begin
      Printf.bprintf b "{\n";
      Printf.bprintf b "%s st_name = %S;\n" indent st.st_name;
      Printf.bprintf b "%s st_value = %s;\n" indent (string_of_addr st.st_value);
      Printf.bprintf b "%s st_size = %Ld;\n" indent st.st_size;
      Printf.bprintf b "%s st_bind = %s;\n" indent
        (string_of_symbol_bind_type st.st_bind);
      Printf.bprintf b "%s st_type = %s;\n" indent
        (string_of_symbol_type st.st_type);
      (*    Printf.bprintf b "%s st_other = %s;\n" indent st.st_other; *)
      Printf.bprintf b "%s st_section = %s;\n" indent
        (match st.st_section with
         | SYM_SHN_UNDEF -> "SYM_SHN_UNDEF"
         | SYM_SHN_ABS -> "SYM_SHN_ABS"
         | SYM_SHN_COMMON -> "SYM_SHN_COMMON"
         | SYM_SHN s -> Printf.sprintf "SYM_SHN %S" s
        );
      Printf.bprintf b "%s}" indent
    end

  let prog_bits b indent sht =
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s sht_flags = [" indent;
      List.iter (fun f ->
        Printf.bprintf b " %s;" (string_of_section_header_flag f))
        sht.sht_flags;
      Printf.bprintf b "];\n";
    Printf.bprintf b "%s sht_addr = %s;\n" indent (string_of_addr sht.sht_addr);
(*    Printf.bprintf b "%s sht_size = %d;\n" indent sht.sht_size; *)
    Printf.bprintf b "%s sht_align = %d;\n" indent sht.sht_align;
    Printf.bprintf b "%s sht_content = string[%d];\n" indent
      (String.length sht.sht_content);
    Printf.bprintf b "%s}" indent

  let string_of_elf64_relocation = function
    | R_X86_64_NONE         -> "R_X86_64_NONE"
    | R_X86_64_64           -> "R_X86_64_64"
    | R_X86_64_PC32         -> "R_X86_64_PC32"
    | R_X86_64_GOT32        -> "R_X86_64_GOT32"
    | R_X86_64_PLT32        -> "R_X86_64_PLT32"
    | R_X86_64_COPY         -> "R_X86_64_COPY"
    | R_X86_64_GLOB_DAT     -> "R_X86_64_GLOB_DAT"
    | R_X86_64_JUMP_SLOT    -> "R_X86_64_JUMP_SLOT"
    | R_X86_64_RELATIVE     -> "R_X86_64_RELATIVE"
    | R_X86_64_GOTPCREL     -> "R_X86_64_GOTPCREL"
    | R_X86_64_32           -> "R_X86_64_32"
    | R_X86_64_32S          -> "R_X86_64_32S"
    | R_X86_64_16           -> "R_X86_64_16"
    | R_X86_64_PC16         -> "R_X86_64_PC16"
    | R_X86_64_8            -> "R_X86_64_8"
    | R_X86_64_PC8          -> "R_X86_64_PC8"
    | R_X86_64_DPTMOD64     -> "R_X86_64_DPTMOD64"
    | R_X86_64_DTPOFF64     -> "R_X86_64_DTPOFF64"
    | R_X86_64_TPOFF64      -> "R_X86_64_TPOFF64"
    | R_X86_64_TLSGD        -> "R_X86_64_TLSGD"
    | R_X86_64_TLSLD        -> "R_X86_64_TLSLD"
    | R_X86_64_DTPOFF32     -> "R_X86_64_DTPOFF32"
    | R_X86_64_GOTTPOFF     -> "R_X86_64_GOTTPOFF"
    | R_X86_64_TPOFF32      -> "R_X86_64_TPOFF32"
    | R_X86_64_PC64         -> "R_X86_64_PC64"
    | R_X86_64_GOTOFF64     -> "R_X86_64_GOTOFF64"
    | R_X86_64_GOTPC32      -> "R_X86_64_GOTPC32"
    | R_X86_64_SIZE32       -> "R_X86_64_SIZE32"
    | R_X86_64_SIZE64       -> "R_X86_64_SIZE64"
    | R_X86_64_UNKNOWN n    -> Printf.sprintf "R_X86_64_UNKNOWN %d" n

  let string_of_elf32_relocation = function
    | R_386_NONE -> "R_386_NONE"
    | R_386_32 -> "R_386_32"
    | R_386_PC32 -> "R_386_PC32"
    | R_386_GOT32 -> "R_386_GOT32"
    | R_386_PLT32 -> "R_386_PLT32"
    | R_386_COPY -> "R_386_COPY"
    | R_386_GLOB_DAT -> "R_386_GLOB_DAT"
    | R_386_JMP_SLOT -> "R_386_JMP_SLOT"
    | R_386_RELATIVE -> "R_386_RELATIVE"
    | R_386_GOTOFF   -> "R_386_GOTOFF"
    | R_386_GOTPC    -> "R_386_GOTPC"
    | R_386_32PLT    -> "R_386_32PLT"
    | R_386_16 -> "R_386_16"
    | R_386_PC16 -> "R_386_PC16"
    | R_386_8    -> "R_386_8"
    | R_386_PC8  -> "R_386_PC8"
    | R_386_UNKNOWN n -> Printf.sprintf "R_386_UNKNOWN %d" n

  let relocations e string_of_reloc_type b relA indent
      { rela_symbols; rela_relocs; rela_target } =

    Printf.bprintf b "SHT_REL%s {\n" (if relA then "A" else "");
    Printf.bprintf b "%s rela_symbols = %S;\n" indent rela_symbols;
    let symbols = StringMap.find rela_symbols e.e_sections in
    Printf.bprintf b "%s rela_target = %S;\n" indent rela_target;
    Printf.bprintf b "%s rela_relocs = [| (* %d relocs *)\n" indent (Array.length rela_relocs);
    Printf.bprintf b "(* if unspecified, r_addend = 0 *)\n";
    Array.iter (fun r ->
      if !compact then begin
        Printf.bprintf b " {" ;
        Printf.bprintf b " r_offset = %Ld;" r.r_offset;
        Printf.bprintf b " r_sym = %Ld;" r.r_sym;
        (match symbols.s_desc with
           SHT_SYMTAB syms ->
           Printf.bprintf b "(* %S *)" syms.(Int64.to_int r.r_sym).st_name
         | _ -> ());
        Printf.bprintf b " r_type = %s;" (string_of_reloc_type r.r_type);
        if r.r_addend <> 0L then
          Printf.bprintf b " r_addend = %Ld;" r.r_addend;
        Printf.bprintf b " };\n"
      end else begin
        Printf.bprintf b "%s {\n" indent;
        Printf.bprintf b "%s  r_offset = %Ld;\n" indent r.r_offset;
        Printf.bprintf b "%s  r_sym = %Ld;" indent r.r_sym;
        (match symbols.s_desc with
           SHT_SYMTAB syms ->
           Printf.bprintf b "(* %S *)" syms.(Int64.to_int r.r_sym).st_name
         | _ -> ());
        Printf.bprintf b "\n";
        Printf.bprintf b "%s  r_type = %s;\n" indent
          (string_of_reloc_type r.r_type);
        Printf.bprintf b "%s  r_addend = %Ld;\n" indent r.r_addend;
        Printf.bprintf b "%s };\n" indent
      end
    ) rela_relocs;
    Printf.bprintf b "|]"

  let section_desc e b indent desc =
    match desc with
    | SHT_STRTAB s ->
      Printf.bprintf b "SHT_STRTAB %d\n" (String.length s);
      if !strtab then begin
        let rec iter pos =
          let pos2 = String.index_from s pos '\000' in
          Printf.bprintf b "%s %d -> %S;\n" indent pos (String.sub s pos (pos2-pos));
          iter (pos2+1)
        in
        try
          Printf.bprintf b "%s {{\n" indent;
          iter 0
        with _ ->
          Printf.bprintf b "%s }}" indent

      end

    | SHT_NOBITS bits ->
      Printf.bprintf b "SHT_NOBITS";
      prog_bits b indent bits
    | SHT_PROGBITS bits ->
      Printf.bprintf b "SHT_PROGBITS";
      prog_bits b indent bits
    | SHT_SYMTAB syms ->
      Printf.bprintf b "SHT_SYMTAB [| (* %d symbols *)\n" (Array.length syms);
      Array.iter (fun sym ->
(*        Printf.bprintf b "%s" indent; *)
        symbol b (indent ^ "  ") sym;
        Printf.bprintf b ";\n"
      ) syms;
      Printf.bprintf b "%s|]" indent
    | SHT_REL32 rela ->
      relocations e string_of_elf32_relocation b false indent rela
    | SHT_RELA32 rela ->
      relocations e string_of_elf32_relocation b true indent rela
    | SHT_REL64 rela ->
      relocations e string_of_elf64_relocation b false indent rela
    | SHT_RELA64 rela ->
      relocations e string_of_elf64_relocation b true indent rela
    | SHT_NULL ->
      Printf.bprintf b "SHT_NULL"
    | SHT_UNKNOWN sec ->
      Printf.bprintf b "SHT_UNKNOWN ";
      RAW.section b (indent ^ "    ") sec
    | SHT_VERSYM versions ->
      if versions = [||] then
        Printf.bprintf b "SHT_GNU_versym [||]"
      else
        let first = ref 0 in
        let version = ref versions.(0) in
        Printf.bprintf b "SHT_GNU_versym [|\n";
        let flush n =
          if n = !first then
            Printf.bprintf b "%s  %d -> %d;\n" indent n !version
          else
            Printf.bprintf b "%s  %d-%d -> %d;\n" indent !first n !version
        in
        Array.iteri (fun i v ->
          if v <> !version then begin
            flush (i-1);
            first := i;
            version := v;
          end
        ) versions;
        flush (Array.length versions - 1);
        Printf.bprintf b "%s|]" indent
    | SHT_VERNEED (strtab, vns) ->
      Printf.bprintf b "SHT_GNU_verneed (%S, [\n%s" strtab indent;
      let indent2 = indent ^ "  " in
      List.iter (fun (vn, vnas) ->
        RAW.verneed b indent vn;
        Printf.bprintf b ", [\n%s" indent;
        List.iter (fun vna ->
          RAW.vernaux b indent2 vna;
        ) vnas;
      Printf.bprintf b "];\n%s" indent;
      ) vns;
      Printf.bprintf b "%s])" indent

  let section e b indent s =
    Printf.bprintf b "{\n";
    Printf.bprintf b "%s s_name = %S;\n" indent s.s_name;
    Printf.bprintf b "%s s_desc = " indent;
    section_desc e b (indent ^ "    ") s.s_desc;
    Printf.bprintf b ";\n";
    Printf.bprintf b "%s}" indent

  let program_desc e b indent = function
    | PR_UNKNOWN -> Printf.bprintf b "PR_UNKNOWN"
    | PR_INTERP s -> Printf.bprintf b "PR_INTERP %S" s

  let program e b indent pp =
    let indent2 = indent ^ "  " in

    Printf.bprintf b "{\n";
    Printf.bprintf b "%s p_header = " indent;
    RAW.program_header b indent2 pp.p_header;
    Printf.bprintf b ";\n";
    Printf.bprintf b "%s p_desc = " indent;
    program_desc e b (indent2 ^ "  ") pp.p_desc;
    Printf.bprintf b ";\n";
    Printf.bprintf b "%s}" indent

let to_ocaml indent e =
  let b = Buffer.create 1000 in

    Printf.bprintf b "{\n";
(*    Printf.bprintf b "%s  e_ident = %S; (* magic number *)\n" indent e.e_ident; *)
    Printf.bprintf b "%s  e_file_class = %s;\n" indent (string_of_file_class e.e_file_class);
    Printf.bprintf b "%s e_data_encoding = %s;\n" indent (string_of_data_encoding e.e_data_encoding);
    Printf.bprintf b "%s e_file_version = %d;\n" indent e.e_file_version;
    Printf.bprintf b "%s e_osabi = %s;\n" indent (string_of_osabi e.e_osabi);
    Printf.bprintf b "%s e_abi_version = %d;\n" indent e.e_abi_version;
    Printf.bprintf b "%s e_type = %s;\n" indent (string_of_type e.e_type);
    Printf.bprintf b "%s e_machine = %s;\n" indent (string_of_machine e.e_machine);
    Printf.bprintf b "%s e_version = %Ld;\n"  indent e.e_version;
    Printf.bprintf b "%s e_entry = %Ld; (* entry point addr *)\n"
      indent e.e_entry;
    Printf.bprintf b "%s e_flags = %Ld;\n" indent e.e_flags;

    Printf.bprintf b "%s e_sections = {|\n" indent;
    StringMap.iter (fun name s ->
      Printf.bprintf b "%s  " indent;
      section e b (indent ^ "    ") s;
      Printf.bprintf b ";\n";
    ) e.e_sections;
    Printf.bprintf b "%s|};\n" indent;

    Printf.bprintf b "%s e_programs = [|\n" indent;
    Array.iteri (fun i pp ->
      Printf.bprintf b "%s %d -> " indent i;
      program e b (indent ^ "    ") pp;
      Printf.bprintf b ";\n";
    ) e.e_programs;
    Printf.bprintf b "%s|];\n" indent;

    Printf.bprintf b "%s}" indent;


  Buffer.contents b

end
