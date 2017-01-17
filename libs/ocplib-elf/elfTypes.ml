(**************************************************************************)
(*                                                                        *)
(*                        OCamlPro Typerex                                *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the LGPL v3.0            *)
(*   (GNU Lesser General Public Licence version 3.0).                     *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)


open StringCompat

module IntMap = Map.Make(struct
    type t = int
    let compare = compare
  end)

type uchar = int64
type addr = int64  (* unsigned *)
type half = int64  (* unsigned 16 bits *)
type off = int64   (* unsigned *)
type sword = int64 (* signed 32 bits *)
type word = int64  (* unsigned *)
type sxword = int64 (* signed 64 bits *)
type xword = int64 (* unsigned 64 bits *)


type byte_order = LittleEndian | BigEndian
type word_size = ARCH32 | ARCH64

type encoding = {
  byte_order : byte_order;
  word_size : word_size;
}

type elf_data_type =
  | ELF_DATA_TYPE_BYTE
  | ELF_DATA_TYPE_ADDR
  | ELF_DATA_TYPE_DYN
  | ELF_DATA_TYPE_EHDR
  | ELF_DATA_TYPE_HALF
  | ELF_DATA_TYPE_OFF
  | ELF_DATA_TYPE_PHDR
  | ELF_DATA_TYPE_RELA
  | ELF_DATA_TYPE_REL
  | ELF_DATA_TYPE_SHDR
  | ELF_DATA_TYPE_SWORD
  | ELF_DATA_TYPE_SYM
  | ELF_DATA_TYPE_WORD
  | ELF_DATA_TYPE_SXWORD
  | ELF_DATA_TYPE_XWORD
  | ELF_DATA_TYPE_VDEF
  | ELF_DATA_TYPE_VNEED
  | ELF_DATA_TYPE_NUM

type elf_type =
  | ET_NONE    (* No file type *)
  | ET_REL     (* relocatable file *)
  | ET_EXEC    (* executable file *)
  | ET_DYN     (* shared object file *)
  | ET_CORE    (* core file *)
  | ET_NUM of int
  | ET_OS of int (* TODO *)
  | ET_PROC of int (* TODO *)

type elf_data_encoding =
  | ELFDATANONE
  | ELFDATA2LSB
  | ELFDATA2MSB
  | ELFDATANUM of int

type elf_file_class =
  | ELFCLASSNONE
  | ELFCLASS32
  | ELFCLASS64
  | ELFCLASSNUM of int

type machine =
    EM_NONE          (* No machine *)
  | EM_M32           (* AT&T WE 32100 *)
  | EM_SPARC         (* SPARC *)
  | EM_386           (* Intel 80386 *)
  | EM_68K           (* Motorola 68000 *)
  | EM_88K           (* Motorola 88000 *)
  | EM_486           (* Intel i486 (DO NOT USE THIS ONE) *)
  | EM_860           (* Intel 80860 *)
  | EM_MIPS          (* MIPS I Architecture *)
  | EM_S370          (* IBM System/370 Processor *)
  | EM_MIPS_RS3_LE   (* MIPS RS3000 Little-endian *)
  | EM_SPARC64       (* SPARC 64-bit *)
  | EM_PARISC        (* Hewlett-Packard PA-RISC *)
  | EM_VPP500        (* Fujitsu VPP500 *)
  | EM_SPARC32PLUS   (* Enhanced instruction set SPARC *)
  | EM_960           (* Intel 80960 *)
  | EM_PPC           (* PowerPC *)
  | EM_PPC64         (* 64-bit PowerPC *)
  | EM_S390          (* IBM System/390 Processor *)
  | EM_SPU           (* Cell SPU *)
  | EM_V800          (* NEC V800 *)
  | EM_FR20          (* Fujitsu FR20 *)
  | EM_RH32          (* TRW RH-32 *)
  | EM_RCE           (* Motorola RCE *)
  | EM_ARM           (* Advanced RISC Machines ARM *)
  | EM_ALPHA         (* Digital Alpha *)
  | EM_SH            (* Hitachi SH *)
  | EM_SPARCV9       (* SPARC Version 9 *)
  | EM_TRICORE       (* Siemens TriCore embedded processor *)
  | EM_ARC           (* Argonaut RISC Core, Argonaut Technologies Inc. *)
  | EM_H8_300        (* Hitachi H8/300 *)
  | EM_H8_300H       (* Hitachi H8/300H *)
  | EM_H8S           (* Hitachi H8S *)
  | EM_H8_500        (* Hitachi H8/500 *)
  | EM_IA_64         (* Intel IA-64 processor architecture *)
  | EM_MIPS_X        (* Stanford MIPS-X *)
  | EM_COLDFIRE      (* Motorola ColdFire *)
  | EM_68HC12        (* Motorola M68HC12 *)
  | EM_MMA           (* Fujitsu MMA Multimedia Accelerator *)
  | EM_PCP           (* Siemens PCP *)
  | EM_NCPU          (* Sony nCPU embedded RISC processor *)
  | EM_NDR1          (* Denso NDR1 microprocessor *)
  | EM_STARCORE      (* Motorola Star*Core processor *)
  | EM_ME16          (* Toyota ME16 processor *)
  | EM_ST100         (* STMicroelectronics ST100 processor *)
  | EM_TINYJ         (* Advanced Logic Corp. TinyJ embedded processor family *)
  | EM_X86_64        (* AMD x86-64 architecture *)
  | EM_PDSP          (* Sony DSP Processor *)
  | EM_FX66          (* Siemens FX66 microcontroller *)
  | EM_ST9PLUS       (* STMicroelectronics ST9+ 8/16 bit microcontroller *)
  | EM_ST7           (* STMicroelectronics ST7 8-bit microcontroller *)
  | EM_68HC16        (* Motorola MC68HC16 Microcontroller *)
  | EM_68HC11        (* Motorola MC68HC11 Microcontroller *)
  | EM_68HC08        (* Motorola MC68HC08 Microcontroller *)
  | EM_68HC05        (* Motorola MC68HC05 Microcontroller *)
  | EM_SVX           (* Silicon Graphics SVx *)
  | EM_ST19          (* STMicroelectronics ST19 8-bit microcontroller *)
  | EM_VAX           (* Digital VAX *)
  | EM_CRIS          (* Axis Communications 32-bit embedded processor *)
  | EM_JAVELIN       (* Infineon Technologies 32-bit embedded processor *)
  | EM_FIREPATH      (* Element 14 64-bit DSP Processor *)
  | EM_ZSP           (* LSI Logic 16-bit DSP Processor *)
  | EM_MMIX          (* Donald Knuth's educational 64-bit processor *)
  | EM_HUANY         (* Harvard University machine-independent object files *)
  | EM_PRISM         (* SiTera Prism *)
  | EM_AVR           (* Atmel AVR 8-bit microcontroller *)
  | EM_FR30          (* Fujitsu FR30 *)
  | EM_D10V          (* Mitsubishi D10V *)
  | EM_D30V          (* Mitsubishi D30V *)
  | EM_V850          (* NEC v850 *)
  | EM_M32R          (* Mitsubishi M32R *)
  | EM_MN10300       (* Matsushita MN10300 *)
  | EM_MN10200       (* Matsushita MN10200 *)
  | EM_PJ            (* picoJava *)
  | EM_OPENRISC      (* OpenRISC 32-bit embedded processor *)
  | EM_ARC_A5        (* ARC Cores Tangent-A5 *)
  | EM_XTENSA        (* Tensilica Xtensa Architecture *)
  | EM_VIDEOCORE     (* Alphamosaic VideoCore processor *)
  | EM_TMM_GPP       (* Thompson Multimedia General Purpose Processor *)
  | EM_NS32K         (* National Semiconductor 32000 series *)
  | EM_TPC           (* Tenor Network TPC processor *)
  | EM_SNP1K         (* Trebia SNP 1000 processor *)
  | EM_ST200         (* STMicroelectronics (www.st.com) ST200 microcontroller *)
  | EM_IP2K          (* Ubicom IP2xxx microcontroller family *)
  | EM_MAX           (* MAX Processor *)
  | EM_CR            (* National Semiconductor CompactRISC microprocessor *)
  | EM_F2MC16        (* Fujitsu F2MC16 *)
  | EM_MSP430        (* Texas Instruments embedded microcontroller msp430 *)
  | EM_BLACKFIN      (* Analog Devices Blackfin (DSP) processor *)
  | EM_SE_C33        (* S1C33 Family of Seiko Epson processors *)
  | EM_SEP           (* Sharp embedded microprocessor *)
  | EM_ARCA          (* Arca RISC Microprocessor *)
  | EM_UNICORE       (* Microprocessor series from PKU-Unity Ltd. and MPRC of Peking University *)
  | EM_NUM of int

type elf_osabi =
  | ELFOSABI_NONE
  | ELFOSABI_SYSV
  | ELFOSABI_HPUX
  | ELFOSABI_NETBSD
  | ELFOSABI_LINUX
  | ELFOSABI_SOLARIS
  | ELFOSABI_AIX
  | ELFOSABI_IRIX
  | ELFOSABI_FREEBSD
  | ELFOSABI_TRU64
  | ELFOSABI_MODESTO
  | ELFOSABI_OPENBSD
  | ELFOSABI_OPENVMS
  | ELFOSABI_NSK
  | ELFOSABI_AROS
  | ELFOSABI_ARM
  | ELFOSABI_STANDALONE
  | ELFOSABI_NUM of int

type section_attribute =
  | SHF_WRITE       (* writable data during execution *)
  | SHF_ALLOC       (* section is in memory during execution *)
  | SHF_EXECINSTR   (* executable machine instructions *)
  | SHF_NUM of int
(*
    | SHF_MERGE
    | SHF_STRINGS
    | SHF_INFO_LINK
    | SHF_LINK_ORDER
    | SHF_OS_NONCONFORMING
    | SHF_GROUP
    | SHF_TLS
    | SHF_MASKOS
    | SHF_MASKPROC
*)


  let standard_sections = [
    ".bss", "Data segment initialized at 0";
    ".comment", "Version Control information";
    ".data", "Initialized data";
    ".debug", "Debug data";
    ".dynamic", "Dynamic linking tables";
    ".dynstr",  "A String table for .dynamic section";
    ".dynsym", "A Symbol table for dynamic linking";
    ".fini", "Code to run on exit";
    ".got", "mach. dep. Global offset table";
    ".hash", "A Symbol hash table";
    ".init", "Code to run on entry";
    ".interp", "Program interpreter path name";
    ".line", "????";
    ".note", "Note section";
    ".plt", "mach. dep. Procedure linkage table";
    ".rel{name}", "Relocations for section {name} (.text for e.g.)";
    ".rela{name}", "Relocations for section {name} (.text for e.g.)";
    ".rodata",  "A Read-only data (constants and literals)";
    ".shstrtab", "Section name string table";
    ".strtab", "String table";
    ".symtab",  "Linker symbol table";
    ".text", "Executable code";
  ]


  type elf_symbol_bind_type =
    | STB_LOCAL  (* local symbol *)
    | STB_GLOBAL (* global symbol *)
    | STB_WEAK   (* low precedence global symbol *)
    | STB_OS of int
    | STB_PROC of int
    | STB_NUM of int

  type elf_symbol_type =
    | STT_NOTYPE     (* no type associated (e.g. absolute symbold) *)
    | STT_OBJECT     (* data object *)
    | STT_FUNC       (* function entry point *)
    | STT_SECTION    (* symbol asssociated with a section *)
    | STT_FILE       (* source file associated with the object file *)
          (* must have STB_LOCAL binding, SHN_ABS for section index and before all local symbols of the file *)
    | STT_OS of int
    | STT_PROC of int
    | STT_NUM of int


type segment_type = (* p_type *)
  | PT_NULL           (* 0 : unused entry *)
  | PT_LOAD           (* 1 : loadable segment *)
  | PT_DYNAMIC        (* 2 : dynamic linking tables *)
  | PT_INTERP         (* 3 : program interpreter path name *)
  | PT_NOTE           (* 4 : note sections *)
  | PT_SHLIB          (* 5 : reserved *)
  | PT_PHDR           (* 6 : program header table *)
  | PT_OS of int64      (* 0x6000_0000 - 0x6FFF_FFFF *)
  | PT_PROC of int64    (* 0x7000_0000 - 0x7FFF_FFFF *)
  | PT_NUM of int64

  | PT_GNU_EH_FRAME

  | PT_GNU_STACK
  (* The p_flags member specifies the permissions on the segment containing the stack and is used to indicate wether the stack should be executable. The absense of this header indicates that the stack will be executable. *)

  | PT_GNU_RELRO
  (* The array element specifies the location and size of a segment
     which may be made read-only after relocation shave been processed. *)

type segment_attribute = (* p_flags *)
    | PF_X              (* 0x1 : execute *)
    | PF_W              (* 0x2 : write *)
    | PF_R              (* 0x4 : read *)
    | PF_OS of int      (* 0x00FF_0000 *)
    | PF_PROC of int    (* 0xFF00_0000 *)
    | PF_NUM of int


type str =
  | ES of int64 * string (* embedded string, i.e. strtabl content + offset *)
  | S of string

let section_name_gnu_version = ".gnu.version"
let section_name_gnu_version_d = ".gnu.version_d"
let section_name_gnu_version_r = ".gnu.version_r"

module RAW = struct

(* ".gnu.version" : for each symbol in ".dynsym", an Elfxx_Half[n].
   0 -> LOCAL symbol
   1 -> GLOBAL symbol
   Other values are to be searched for in the ".gnu.version_r" section,
   where each [verneed] entry contains several [vernaux] structures, each one
   with a [vna_other] field containing the identifier to be used here. *)


(*
vd_version Version revision. This field shall be set to 1.
vd_flags   Version information flag bitmask.
vd_ndx 	   Version index numeric value referencing the SHT_GNU_versym section.
vd_cnt 	   Number of associated verdaux array entries.
vd_hash    Version name hash value (ELF hash function).
vd_aux	   Offset in bytes to a corresponding entry in an array of
             Elfxx_Verdaux structures as defined in Figure 11-2
vd_next    Offset to the next verdef entry, in bytes.
*)
type verdef = {
  vd_version : int64;
  vd_flags : int64;
  vd_ndx : int64;
  vd_cnt : int64;
  vd_hash : int64;
  vd_aux : int64;
  vd_next : int64;
}

(*
vda_name  Offset to the version or dependency name string in the
              section header, in bytes.
vda_next  Offset to the next verdaux entry, in bytes.
*)
type verdaux = {
  vda_name : int64;
  vda_next : int64;
}

(*
vn_version   Version of structure. This value is currently set to 1, and
     will be reset if the versioning implementation is incompatibly altered.
vn_cnt       Number of associated verneed array entries.
vn_file      Offset to the file name string in the section header, in bytes.
vn_aux       Offset to a corresponding entry in the vernaux array, in bytes.
vn_next      Offset to the next verneed entry, in bytes.
*)
type verneed = {
  vn_version : int64;
  vn_cnt : int64;
  vn_file : str;
  vn_aux : int64;
  vn_next : int64;
}

(*
vna_hash  Dependency name hash value (ELF hash function).
vna_flags Dependency information flag bitmask.
vna_other Object file version identifier used in the .gnu.version symbol
       version array. Bit number 15 controls whether or not the object is
       hidden; if this bit is set, the object cannot be used and the static
       linker will ignore the symbol's presence in the object.
vna_name  Offset to the dependency name string in the section header, in bytes.
vna_next  Offset to the next vernaux entry, in bytes.
*)
type vernaux =  {
  vna_hash : int64;
  vna_flags : int64;
  vna_other : int64;
  vna_name : str;
  vna_next : int64;
}

type elf_section_type =
  | SHT_NULL        (* useless section *)
  | SHT_PROGBITS    (* can only be used by the program itself *)
  | SHT_SYMTAB      (* symbol table (for linking) *)
  | SHT_STRTAB      (* string table *)
  | SHT_RELA        (* relocation entries with explicit addends *)
  | SHT_HASH        (* symbol hash table, required for dynamic linking *)
  | SHT_DYNAMIC     (* info for dynamic linking *)
  | SHT_NOTE        (* note on the file *)
  | SHT_NOBITS      (* contains nothing in the file, but probably allocated in memory *)
  | SHT_REL         (* relocation entries without explicit addends *)
  | SHT_SHLIB       (* reserved ? *)
  | SHT_DYNSYM      (* symbol table (for linking) *)
  | SHT_INIT_ARRAY
  | SHT_FINI_ARRAY
  | SHT_PREINIT_ARRAY
  | SHT_GROUP
  | SHT_SYMTAB_SHNDX
  | SHT_NUM of int
  | SHT_OS of int (* TODO *)
  | SHT_PROC of int (* TODO *)
  | SHT_USER of int (* TODO *)
  | SHT_GNU_verdef (* This section contains the symbol versions that are provided. *)
  | SHT_GNU_verneed (* This section contains the symbol versions that are required. *)
  | SHT_GNU_versym (* This section contains the Symbol Version Table. *)

  type section_header = {
    sh_name : word;               (* name of section, index in String Table *)
    sh_type : elf_section_type;   (* section type and semantics *)
    sh_flags : section_attribute list;  (* 1-bit flags *)
    sh_addr : addr;               (* required position in memory, or 0 *)
    sh_offset : off;              (* position of the section *)
    sh_size : word;               (* size of section *)
    sh_link : word;
    (* index of a link to another section header in the section header
       table *)
    (* sh_type => associated section
       - SHT_DYNAMIC String table used by entries in this section
       - SHT_HASH Symbol table to which the hash table applies
       - SHT_REL
       - SHT_RELA  Symbol table referenced by relocations
       - SHT_SYMTAB
       - SHT_DYNSYM String table used by entries in this section
    *)
    sh_info : word;               (* extra info *)
    (* sh_type => meaining of sh_info
       - SHT_REL
       - SHT_RELA Section index of section to which the relocations apply
       - SHT_SYMTAB
       - SHT_DYNSYM Index of first non-local symbol (i.e., number of local symbols)
    *)
    sh_addralign : word;
    (* alignment needed by this section 0,1,2,4,8, 16, etc *)
    sh_entsize : word;
    (* entry size if the section is a table *)
  }

  type section = {
    section_name : string;
    section_content : string;
    section_header : section_header;
  }

  type program_header = {
    p_type : segment_type;      (* type of segment  *)
    p_flags : word;     (* segment attributes *)
(* : segment attribute list *)
    p_offset : off;     (* offset in file *)
    p_vaddr : addr;     (* virtual address in memory *)
    p_paddr : addr;     (* reserved, physical addressing *)
    p_filesz : xword;   (* size of segment in file *)
    p_memsz : xword;    (* size of segment in memory *)
    p_align : xword;    (* alignment of segment *)
  }

  type program = {
    program_content : string; (* the content of the corresponding segment *)
    program_header : program_header;
  }

  type header = {
    e_ident : string;          (* 16 bytes ELF identification (magic string) *)
    e_file_class : elf_file_class;
    e_data_encoding : elf_data_encoding;
    e_file_version : int;
    e_osabi : elf_osabi;
    e_abi_version : int;

    e_type      : elf_type;  (* object file type *)
    e_machine   : machine;   (* machine type *)
    e_version   : word;      (* object file version (should be 1) *)
    e_entry     :   addr;    (* entry point address (or 0) *)
    e_phoff     :   off;     (* program header table position, or 0 *)
    e_shoff     :   off;     (* section header table position, or 0 *)
    e_flags     :   word;    (* processor specific flags *)
    e_ehsize    :   half;    (* elf header size in bytes *)
    e_phentsize :   half;    (* size of one entry in program header table *)
    e_phnum     :   half;    (* number of entries in program header table *)
    e_shentsize :   half;    (* size of one entry in section header table *)
    e_shnum     :   half;    (* number of entries in section header table *)
    e_shstrndx  :   half;    (* section name string table index in
                             section header table, or SHN_UNDEF *)
  }

  type t = {
    elf_content : string; (* the complete file *)
    elf_header : header;
    elf_programs : program array;  (* needed for execution *)
    elf_sections : section array;
  }

end

module ABSTRACT = struct

  type symbol_section =
    | SYM_SHN_UNDEF
    | SYM_SHN_ABS
    | SYM_SHN_COMMON
    | SYM_SHN of string



type elf_symbol = {
  st_name : string; (* st_name : word *)
  st_value : addr;  (* symbol value : absolute and relocatable address
                       relocatable files: alignment for commons,
                       or offset from begin of section for defined symbols
                       executable/shared files: virtual address for defined
                       relocatable symbols      *)
  st_size : word;   (* size of object (e.g. common)
                       0 if no associated size, or size unknown *)
  (*    st_info : uchar;                   (* type and binding attributes *) *)
  st_bind : elf_symbol_bind_type;       (* st_info bind value *)
  st_type : elf_symbol_type;            (* st_info type value *)
  (*    st_other : uchar;                  (* reserved *) *)
  (* st_shndx : half; *)
  (* section table index, where the symbol is defined
     if the symbol is undefined, it is SHN_UNDEF
     if the symbol is absolute, it is SHN_ABS
     if the symbol is common, it is SHN_COMMON
  *)
  st_section : symbol_section;
}

  type elf64_rel = {
    rel_offset : addr;
    (* address of reference relocatable file: offset since beginning of
                                 section executable/shared: virtual
                                 address *)
    rel_info : xword;      (* symbol index and type of relocation *)
    rel_info_symbol : half;
    (* symbol (higher 32 bits on 64 bits, higher 24 bits on 32 bits) *)
    rel_info_type : half;
    (* type (lower 32 bits on 64 bits, lower 8 bits on 32 bits) *)
  }

  type elf64_rela = {
    rela_offset : addr;     (* address of reference *)
    rela_info : xword;      (* symbold index and type of relocation *)
    rela_info_symbol : half;
    (* symbol (higher 32 bits on 64 bits, higher 24 bits on 32 bits) *)
    rela_info_type : half;
    (* type (lower 32 bits on 64 bits, lower 8 bits on 32 bits) *)
    rela_addend : sxword;
    (* constant part of expression (to be added before storing) *)
  }

  type elf32_rel = {
    rel_offset : addr;
    (* address of reference relocatable file: offset since beginning
                             of section executable/shared: virtual
                             address *)
    rel_info : xword;      (* symbol index and type of relocation *)
    rel_info_symbol : half;
    (* symbol (higher 32 bits on 64 bits, higher 24 bits on 32 bits) *)
    rel_info_type : half;
    (* type (lower 32 bits on 64 bits, lower 8 bits on 32 bits) *)
  }

  type elf32_rela = {
    rela_offset : addr;     (* address of reference *)
    rela_info : word;      (* symbold index and type of relocation *)
    rela_info_symbol : half;
    (* symbol (higher 32 bits on 64 bits, higher 24 bits on 32 bits) *)
    rela_info_type : half;
    (* type (lower 32 bits on 64 bits, lower 8 bits on 32 bits) *)
    rela_addend : sword;
    (* constant part of expression (to be added before storing) *)
  }

  type note_section = { (* or note_segment *)
    notes : xword array;
    note_namez : xword; (* size of name *)
    note_descsz : xword; (* size of description *)
    note_type : xword;  (* type, originator dependent *)
    note_name : string;
    (* 0-terminated name, size does not include final 0, 8-padded in
       file *)
    note_desc : string; (* note content, 8-padded in file *)
  }

  type dynamic_tag =
      DT_NULL      (* 0 ignored Marks the end of the dynamic array *)
    | DT_NEEDED
    (* 1 d_val The string table offset of the name of a needed
       library. *)
    | DT_PLTRELSZ
    (* 2 d_val Total size, in bytes, of the relocation entries
                         associated with the procedure linkage table. *)
    | DT_PLTGOT
    (* 3 d_ptr Contains an address associated with the linkage
                       table. The specific meaning of this field is
                       processor-dependent. *)
    | DT_HASH
    (* 4 d_ptr Address of the symbol hash table, described below. *)
    | DT_STRTAB    (* 5 d_ptr Address of the dynamic string table. *)
    | DT_SYMTAB    (* 6 d_ptr Address of the dynamic symbol table. *)
    | DT_RELA
    (* 7 d_ptr Address of a relocation table with Elf64_Rela
       entries. *)
    | DT_RELASZ
    (* 8 d_val Total size, in bytes, of the DT_RELA relocation
       table. *)
    | DT_RELAENT
    (* 9 d_val Size, in bytes, of each DT_RELA relocation entry. *)
    | DT_STRSZ     (* 10 d_val Total size, in bytes, of the string table. *)
    | DT_SYMENT    (* 11 d_val Size, in bytes, of each symbol table entry. *)
    | DT_INIT      (* 12 d_ptr Address of the initialization function. *)
    | DT_FINI      (*  13 d_ptr Address of the termination function. *)
    | DT_SONAME
    (* 14 d_val The string table offset of the name of this shared
       object. *)
    | DT_RPATH
    (* 15 d_val The string table offset of a shared library search
       path string. *)
    | DT_SYMBOLIC
    (* 16 ignored The presence of this dynamic table entry modifies
                      the symbol resolution algorithm for references
                      within the library. Symbols defined within the
                      library are used to resolve references before
                      the dynamic linker searches the usual search
                      path. *)
    | DT_REL
    (* 17 d_ptr Address of a relocation table with Elf64_Rel
       entries. *)
    | DT_RELSZ
    (* 18 d_val Total size, in bytes, of the DT_REL relocation
       table. *)
    | DT_RELENT
    (* 19 d_val Size, in bytes, of each DT_REL relocation entry. *)
    | DT_PLTREL
    (* 20 d_val Type of relocation entry used for the procedure
                      linkage table. The d_val member contains either
                      DT_REL or DT_RELA. *)
    | DT_DEBUG     (* 21 d_ptr Reserved for debugger use. *)
    | DT_TEXTREL
    (* 22 ignored The presence of this dynamic table entry signals that
                       the relocation table contains relocations for a
                       non-writable segment. *)
    | DT_JMPREL
    (* 23 d_ptr Address of the relocations associated with the
                      procedure linkage table. *)
    | DT_BIND_NOW
    (* 24 ignored The presence of this dynamic table entry signals that
                        the dynamic loader should process all
                        relocations for this object before transferring
                        control to the program. *)
    | DT_INIT_ARRAY
    (* 25 d_ptr Pointer to an array of pointers to initialization
       functions. *)
    | DT_FINI_ARRAY
    (* 26 d_ptr Pointer to an array of pointers to termination
       functions. *)
    | DT_INIT_ARRAYSZ
    (* 27 d_val Size, in bytes, of the array of initialization
       functions. *)
    | DT_FINI_ARRAYSZ
    (* 28 d_val Size, in bytes, of the array of termination functions. *)
    | DT_OS of int
    (* 0x60000000 - 0x6FFF_FFFF Defines a range of dynamic table tags
                         that are reserved for environment-specific
                         use. *)
    | DT_PROC of int
    (* 0x7000_0000 - 0x7FFF_FFFF Defines a range of dynamic table tags
                           that are reserved for processor-specific
                           use. *)
    | DT_NUM of int

  type dynamic = { (* ".dynamic" *)
    d_tag : sxword;  (* dynamic tag *)
    d_val : xword; (* for integer values, or d_ptr for virtual pointers *)
  }

  (* ".hash" section *)
  type hash_section = {
    hash_words : word array;
    hash_nbuckets : word;
    hash_nchain: word; (* also the number of symbols entries stored *)
    hash_buckets : word array;
    hash_chains : word array;
    (* the bucket contains the symbol number associated with the hash
       value, and the next symbol in the collision list is found in
       the chain at the index of the symbol number, until STN_UNDEF is
       found. *)
  }

(*
  unsigned long
  elf64_hash(const unsigned char *name)
  {
  unsigned long h = 0, g;
  while ( *name ) {
  h = (h << 4) + *name++;
  if (g = h & 0xf0000000)
  h ^= g >> 24;
  h &= 0x0f f f f f f f ;
  }
  return h;
  }
      *)

(*
  let elf64_hash name =
    let h = ref 0L in
(*    let g = ref 0L in *)
    for i = 0 to String.length name - 1 do
      let c = int_of_char name.[i] in
      h := Int64.add (Int64.shift_left !h 4) (Int64.of_int c);
      assert false;
    done;
    !h
*)

  type dynamic_segment = dynamic array

  type elf32_relocation =
    | R_386_NONE
    | R_386_32        (* direct,       S + A     *)
    | R_386_PC32      (* PC-relative,  S + A - P *)
    | R_386_GOT32     (* GOT entry,    G + A     *)
    | R_386_PLT32     (* PLT entry,    L + A - P *)
    | R_386_COPY
    | R_386_GLOB_DAT  (* create GOT entry, S *)
    | R_386_JMP_SLOT  (* create PLT entry, S *)
    | R_386_RELATIVE  (* rel. to program base, B + A *)
    | R_386_GOTOFF    (* offset to GOT, S + A - GOT *)
    | R_386_GOTPC     (* GOT + A - P *)
    | R_386_32PLT     (* L + A       *)
(* GNU extensions for LD *)
    | R_386_16       (* = 20 *) (* 16-bit direct,      S + A     *)
    | R_386_PC16     (* = 21 *) (* 16-bit PC-relative, S + A - P *)
    | R_386_8        (* = 22 *) (* 8-bit direct,       S + A     *)
    | R_386_PC8      (* = 23 *) (* 8-bit PC-relative,  S + A - P *)
    | R_386_UNKNOWN of int

  type elf64_relocation =
    | R_X86_64_NONE        (*  =   0 *)
    | R_X86_64_64          (*  =   1 *)    (* S + A     *)
    | R_X86_64_PC32        (*  =   2 *)    (* S + A - P *)
    | R_X86_64_GOT32       (*  =   3 *)    (* G + A     *)
    | R_X86_64_PLT32       (*  =   4 *)    (* L + A - P *)
    | R_X86_64_COPY        (*  =   5 *)    (*           *)
    | R_X86_64_GLOB_DAT    (*  =   6 *)    (* S         *)
    | R_X86_64_JUMP_SLOT   (*  =   7 *)    (* S         *)
    | R_X86_64_RELATIVE    (*  =   8 *)    (* B + A     *)
    | R_X86_64_GOTPCREL    (*  =   9 *)    (* G + GOT + A - P *)
    | R_X86_64_32          (*  =  10 *)    (* S + A     *)
    | R_X86_64_32S         (*  =  11 *)    (* S + A     *)
    | R_X86_64_16          (*  =  12 *)    (* S + A     *)
    | R_X86_64_PC16        (*  =  13 *)    (* S + A - P *)
    | R_X86_64_8           (*  =  14 *)    (* S + A     *)
    | R_X86_64_PC8         (*  =  15 *)    (* S + A - P *)
    | R_X86_64_DPTMOD64    (*  =  16 *)
    | R_X86_64_DTPOFF64    (*  =  17 *)
    | R_X86_64_TPOFF64     (*  =  18 *)
    | R_X86_64_TLSGD       (*  =  19 *)
    | R_X86_64_TLSLD       (*  =  20 *)
    | R_X86_64_DTPOFF32    (*  =  21 *)
    | R_X86_64_GOTTPOFF    (*  =  22 *)
    | R_X86_64_TPOFF32     (*  =  23 *)
    | R_X86_64_PC64        (*  =  24 *)    (* S + A - P   *)
    | R_X86_64_GOTOFF64    (*  =  25 *)    (* S + A - GOT *)
    | R_X86_64_GOTPC32     (*  =  26 *)    (* GOT + A - P *)
    | R_X86_64_SIZE32      (*  =  32 *)
    | R_X86_64_SIZE64      (*  =  33 *)
    | R_X86_64_UNKNOWN of int

  type 'relocation rela_reloc = {
    r_offset : int64;
    r_sym : int64;
    r_type : 'relocation;
    r_addend : int64;
  }

  type 'relocation rela = {
    rela_relocs : 'relocation rela_reloc array;
    rela_symbols : string;
    rela_target : string;
  }

  type section_bits = {
    sht_flags : section_attribute list;
    sht_addr : int64;
    sht_align : int;
    sht_content : string;
  }

  type section_desc =
    | SHT_NULL
    | SHT_PROGBITS of section_bits
    | SHT_NOBITS of section_bits
    | SHT_STRTAB of string
    | SHT_SYMTAB of elf_symbol array
    | SHT_RELA32 of elf32_relocation rela
    | SHT_RELA64 of elf64_relocation rela
    | SHT_REL32 of elf32_relocation rela
    | SHT_REL64 of elf64_relocation rela
    | SHT_VERSYM of int array
    | SHT_VERNEED of string * (RAW.verneed * RAW.vernaux list) list

    | SHT_UNKNOWN of RAW.section

  type section = {
    s_name : string;
    s_desc : section_desc;
  }

  type program_desc =
    | PR_INTERP of string
    | PR_UNKNOWN

  type program = {
    p_header : RAW.program_header;
    mutable p_desc : program_desc;
  }

  type t = {
    e_file_class : elf_file_class;
    e_data_encoding : elf_data_encoding;
    e_file_version : int;
    e_osabi : elf_osabi;
    e_abi_version : int;

    e_type      : elf_type;  (* object file type *)
    e_machine   : machine;   (* machine type *)
    e_version   : word;      (* object file version (should be 1) *)
    e_entry     :   addr;    (* entry point address (or 0) *)
(*
    e_phoff     :   off;     (* program header table position, or 0 *)
    e_shoff     :   off;     (* section header table position, or 0 *)
*)
    e_flags     :   word;    (* processor specific flags *)
(*
  e_ehsize    :   half;    (* elf header size in bytes *)
  e_phentsize :   half;    (* size of one entry in program header table *)
  e_phnum     :   half;    (* number of entries in program header table *)
  e_shentsize :   half;    (* size of one entry in section header table *)
  e_shnum     :   half;    (* number of entries in section header table *)
  e_shstrndx  :   half;    (* section name string table index in
                              section header table, or SHN_UNDEF *)
*)
    mutable e_sections : section StringMap.t;
    mutable e_programs : program array;
  }

end
