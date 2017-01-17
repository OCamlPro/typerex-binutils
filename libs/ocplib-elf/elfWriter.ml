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

(*
Elf32_Addr  4   Unsigned program address
Elf32_Half  2   Unsigned medium integer
Elf32_Off   4   Unsigned file offset
Elf32_Sword 4   Signed large integer
Elf32_Word  4   Unsigned large integer
*)

open StringCompat
open ElfTypes
open ElfTypes.ABSTRACT
open ElfUtils

module R = ElfTypes.RAW

let debug = ref false

type tmp_section = {
  sh_num : int;
  sh_content : string;
  sh_header : R.section_header;
  mutable sh_pos : int;
}

type tmp_strtab = {
  mutable tmpstr_map : int StringMap.t;
  tmpstr_buf : Buffer.t;
}

type tmp = {
  tmp_elf : ElfTypes.ABSTRACT.t;
  tmp_en : ElfTypes.encoding;
  tmp_buf : Buffer.t;
  mutable tmp_pos : int;
  mutable tmp_nsections : int;
  mutable tmp_map : int StringMap.t;
  mutable tmp_sections : tmp_section IntMap.t;
  mutable tmp_contents : string list;
  tmp_strtab : tmp_strtab;
  tmp_shstrtab : tmp_strtab;
}

let create
    e_file_class e_data_encoding
    e_osabi e_machine =
  {
    e_file_class;
    e_data_encoding;
    e_file_version = 1;
    e_osabi;
    e_abi_version = 0;
    e_type = ET_REL;
    e_machine;
    e_version = 1L;
    e_entry = 0L; (* entry point addr *)
    e_flags = 0L;
    e_sections = StringMap.empty;
    e_programs = [||];
  }

type popular_system =
  | S_386_Linux
  | S_X64_Linux

let create_popular = function
  | S_X64_Linux -> create
                     ELFCLASS64
                     ELFDATA2LSB
                     ELFOSABI_SYSV
                     EM_X86_64
  | S_386_Linux -> create
                     ELFCLASS32
                     ELFDATA2LSB
                     ELFOSABI_SYSV
                     EM_386

let new_strtab () = {
  tmpstr_map = StringMap.empty;
  tmpstr_buf = Buffer.create 111;
}

let get_string tab str =
  try
    StringMap.find str tab.tmpstr_map
  with Not_found ->
    (* TODO add compression *)
    if !debug then
      Printf.eprintf "new  string %S\n%!" str;
    let pos = Buffer.length tab.tmpstr_buf in
    tab.tmpstr_map <- StringMap.add str pos tab.tmpstr_map;
    Buffer.add_string tab.tmpstr_buf str;
    Buffer.add_char tab.tmpstr_buf '\000';
    pos

let section_num tmp name =
  try
    StringMap.find name tmp.tmp_map
  with Not_found ->
    let num = tmp.tmp_nsections in
    tmp.tmp_nsections <- tmp.tmp_nsections + 1;
    tmp.tmp_map <- StringMap.add name num tmp.tmp_map;
    if !debug then
      Printf.eprintf "Section %d : %S\n%!" num name;
    num

let int_of_symbol_bind = function
  | STB_LOCAL -> 0
  | STB_GLOBAL -> 1
  | STB_WEAK -> 2
  | STB_OS n -> n+10
  | STB_PROC n -> n+13
  | STB_NUM n -> n

let int_of_symbol_type = function
  | STT_NOTYPE -> 0
  | STT_OBJECT -> 1
  | STT_FUNC -> 2
  | STT_SECTION -> 3
  | STT_FILE -> 4
  | STT_OS n -> n+10
  | STT_PROC n -> n+13
  | STT_NUM n -> n


module RAW = struct

  open RAW
let buf_verdef en b vd  =
  buf_Elfxx_Half en b vd.vd_version;
  buf_Elfxx_Half en b vd.vd_flags;
  buf_Elfxx_Half en b vd.vd_ndx;
  buf_Elfxx_Half en b vd.vd_cnt;
  buf_Elfxx_Word en b vd.vd_hash;
  buf_Elfxx_Word en b vd.vd_aux;
  buf_Elfxx_Word en b vd.vd_next;
  ()

let buf_verdaux en b vda =
  buf_Elfxx_Word en b vda.vda_name;
  buf_Elfxx_Word en b vda.vda_next;
  ()

let int64_of_str s = match s with
    ES (n, _) -> n | S _ -> assert false

let buf_Verneed en b vn =
  buf_Elfxx_Half  en b vn.vn_version;
  buf_Elfxx_Half  en b vn.vn_cnt;
  buf_Elfxx_Word  en b (int64_of_str vn.vn_file);
  buf_Elfxx_Word  en b vn.vn_aux;
  buf_Elfxx_Word  en b vn.vn_next;
  ()

let buf_vernaux en b vna =
  buf_Elfxx_Word en b vna.vna_hash;
  buf_Elfxx_Half en b vna.vna_flags;
  buf_Elfxx_Half en b vna.vna_other;
  buf_Elfxx_Word en b (int64_of_str vna.vna_name);
  buf_Elfxx_Word en b vna.vna_next;
  ()

end

let buf_symbol_table_entry tmp b st =
  if !debug then
    Printf.eprintf "buf_symbol_table_entry[%d]\n%!"
      (Buffer.length b);
  let en = tmp.tmp_en in

  let st_shndx = match st.st_section with
    | SYM_SHN_UNDEF -> shn_undefL
    | SYM_SHN_ABS -> shn_absL
    | SYM_SHN_COMMON -> shn_commonL
    | SYM_SHN s -> Int64.of_int (section_num tmp s) in
  let st_bind = int_of_symbol_bind st.st_bind in
  let st_type = int_of_symbol_type st.st_type in
  let st_info = (st_bind lsl 4) lor st_type in
  let st_name = get_string tmp.tmp_strtab st.st_name in
  if !debug then
    Printf.eprintf "symbol = %S -> %d\n%!" st.st_name st_name;
  if en.word_size = ARCH32 then begin
(*
Elf32_Word    st_name;  (* 4 *)
Elf32_Addr    st_value; (* 4 *)
Elf32_Word    st_size;  (* 4 *)
unsigned char st_info;  (* 1 *)
unsigned char st_other; (* 1 *)
Elf32_Half    st_shndx; (* 2 *)
} Elf32_Sym;
*)
    buf_word32 en b (Int64.of_int st_name);
    buf_addr32 en b st.st_value;
    buf_word32 en b st.st_size;
    Buffer.add_char b (char_of_int st_info);
    Buffer.add_char b '\000' (* st_other *);
    buf_half64 en b st_shndx;

  end else begin
(*
Elf64_Word st_name;  (* 4 *)
unsigned char st_info; (* 1 *)
unsigned char st_other; (* 1 *)
Elf64_Half st_shndx; (* 2 *)
Elf64_Addr st_value; (* 8 *)
Elf64_Xword st_size; (* 8 *)
*)

    buf_word32 en b (Int64.of_int st_name);
    Buffer.add_char b (char_of_int st_info);
    Buffer.add_char b '\000' (* st_other *);
    buf_half64 en b st_shndx;
    buf_addr64 en b st.st_value;
    buf_addr64 en b st.st_size;
    ()
  end

let buf_rel32 en rela buf_addend =
  let b = Buffer.create 10000 in
  Array.iter (fun r ->
    let r_type = match r.r_type with
      | R_386_NONE -> 0
      | R_386_32 -> 1
      | R_386_PC32 -> 2
      | R_386_GOT32 -> 3
      | R_386_PLT32 -> 4
      | R_386_COPY -> 5
      | R_386_GLOB_DAT -> 6
      | R_386_JMP_SLOT -> 7
      | R_386_RELATIVE -> 8
      | R_386_GOTOFF -> 9
      | R_386_GOTPC -> 10
      | R_386_32PLT -> 11
      | R_386_16 -> 20
      | R_386_PC16 -> 21
      | R_386_8 -> 22
      | R_386_PC8 -> 23
      | R_386_UNKNOWN n -> n
    in
    let r_info =
      Int64.logor (Int64.of_int r_type) (Int64.shift_left r.r_sym 8)
    in
    buf_addr32 en b r.r_offset;
    buf_word32 en b r_info;
    buf_addend en b r.r_addend;
  ) rela.rela_relocs;
  Buffer.contents b
(*
typedef struct {
Elf32_Addr r_offset; (* 4 *)
Elf32_Word  r_info;  (* 4 *)
Elf32_Sword  r_addend; (* 4 *)
} Elf32_Rela;

r_info =  (r_sym lsl 8) lor r_type
*)

let int_of_segment = function
  | PT_NULL -> 0L
  | PT_LOAD -> 1L
  | PT_DYNAMIC -> 2L        (* 2 : dynamic linking tables *)
  | PT_INTERP -> 3L         (* 3 : program interpreter path name *)
  | PT_NOTE  -> 4L         (* 4 : note sections *)
  | PT_SHLIB -> 5L          (* 5 : reserved *)
  | PT_PHDR -> 6L           (* 6 : program header table *)

  | PT_GNU_EH_FRAME ->    0x6474e550L
  | PT_GNU_STACK ->   0x6474e551L
  | PT_GNU_RELRO ->   0x6474e552L
  | PT_OS n | PT_PROC n | PT_NUM n -> n


let buf_rel64 en rela buf_addend =
  let b = Buffer.create 10000 in
  Array.iter (fun r ->
    let r_type = match r.r_type with
      |  R_X86_64_NONE ->    0
      |  R_X86_64_64 ->    1
      |  R_X86_64_PC32 ->    2
      |  R_X86_64_GOT32 ->    3
      |  R_X86_64_PLT32 ->    4
      |  R_X86_64_COPY ->    5
      |  R_X86_64_GLOB_DAT ->    6
      |  R_X86_64_JUMP_SLOT ->    7
      |  R_X86_64_RELATIVE ->    8
      |  R_X86_64_GOTPCREL ->    9
      |  R_X86_64_32 ->   10
      |  R_X86_64_32S ->   11
      |  R_X86_64_16 ->   12
      |  R_X86_64_PC16 ->   13
      |  R_X86_64_8 ->   14
      |  R_X86_64_PC8 ->   15
      |  R_X86_64_DPTMOD64 ->   16
      |  R_X86_64_DTPOFF64 ->   17
      |  R_X86_64_TPOFF64 ->   18
      |  R_X86_64_TLSGD ->   19
      |  R_X86_64_TLSLD ->   20
      |  R_X86_64_DTPOFF32 ->   21
      |  R_X86_64_GOTTPOFF ->   22
      |  R_X86_64_TPOFF32 ->   23
      |  R_X86_64_PC64 ->   24
      |  R_X86_64_GOTOFF64 ->   25
      |  R_X86_64_GOTPC32 ->   26
      |  R_X86_64_SIZE32 ->   32
      |  R_X86_64_SIZE64 ->   33
      |  R_X86_64_UNKNOWN n ->  n
    in
    let r_info =
      Int64.logor
        (Int64.of_int r_type) (Int64.shift_left r.r_sym 32)
    in
    buf_addr64 en b r.r_offset;
    buf_word64 en b r_info;
    buf_word64 en b r.r_addend;
  ) rela.rela_relocs;
  Buffer.contents b


let add_section tmp s_name sh_type =
  let en = tmp.tmp_en in
  let sh_num = section_num tmp s_name in
  let sh_name = get_string tmp.tmp_shstrtab s_name in
  let sh_offset = tmp.tmp_pos in
  if !debug then
    Printf.eprintf "Section %S saved at %d\n%!" s_name sh_offset;
  let sh_content = match sh_type with
    | SHT_NULL -> ""
    | SHT_PROGBITS sht -> sht.sht_content
    | SHT_NOBITS sht -> sht.sht_content
    | SHT_STRTAB s -> s
    | SHT_SYMTAB syms ->
      let b = Buffer.create 10000 in
      Array.iter (fun s ->
        buf_symbol_table_entry tmp b s
      ) syms;
      Buffer.contents b
    | SHT_REL32 rela ->
      buf_rel32 en rela (fun en b addend -> assert (addend = 0L))
    | SHT_RELA32 rela -> buf_rel32 en rela buf_word32
    | SHT_REL64 rela ->
      buf_rel64 en rela (fun en b addend -> assert (addend = 0L))
    | SHT_RELA64 rela -> buf_rel64 en rela buf_word64
    | SHT_UNKNOWN sec -> sec.R.section_content

    | SHT_VERNEED _ -> assert false (* TODO *)
    | SHT_VERSYM _ -> assert false (* TODO *)
  in
  let sh_header = {
    R.sh_name = Int64.of_int sh_name;
    sh_size = Int64.of_int (String.length sh_content);
    sh_type = R.SHT_NULL;
    sh_flags = [];
    sh_addr = 0L;
    sh_offset = Int64.of_int sh_offset;
    sh_link = 0L;
    sh_info = 0L;
    sh_addralign = 0L;
    sh_entsize = 0L;
  } in
  let progbits sh_header sht sh_type =
    { sh_header with
      R.sh_flags = sht.sht_flags;
      sh_addr = sht.sht_addr;
      sh_addralign = Int64.of_int sht.sht_align;
      sh_type = sh_type;
    } in

  let after_last_local_symbol symtab =
    let rec iter symtab n =
      if n = 0 || symtab.(n-1).st_bind = STB_LOCAL then n else
        iter symtab (n-1)
    in
    iter symtab (Array.length symtab)
  in

  let sh_header =
    match sh_type with
    | SHT_NULL -> { sh_header with
      R.sh_type = R.SHT_NULL;
      sh_offset = 0L;
    }
    | SHT_UNKNOWN sec ->
      { sec.R.section_header with
        R.sh_offset = sh_header.R.sh_offset;
      }
    | SHT_PROGBITS sht -> progbits sh_header sht R.SHT_PROGBITS
    | SHT_NOBITS sht -> progbits sh_header sht R.SHT_NOBITS
    | SHT_STRTAB s ->
      { sh_header with R.sh_type = R.SHT_STRTAB }
    | SHT_SYMTAB symtab ->
      { sh_header with
        R.sh_type = R.SHT_SYMTAB;
        R.sh_link = Int64.of_int (section_num tmp ".strtab");
        sh_addralign = (
          if en.word_size = ARCH32 then 8L else 8L);
        sh_entsize = (
          if en.word_size = ARCH32 then 16L else 24L);
        sh_info = Int64.of_int (after_last_local_symbol symtab);
      }
    | SHT_REL32 rela ->
      { sh_header with
        R.sh_type = R.SHT_REL;
        R.sh_link = Int64.of_int (section_num tmp rela.rela_symbols);
        R.sh_info = Int64.of_int (section_num tmp rela.rela_target);
        R.sh_entsize = 8L;
        R.sh_addralign = 8L;
      }
    | SHT_RELA32 rela ->
      { sh_header with
        R.sh_type = R.SHT_RELA;
        R.sh_link = Int64.of_int (section_num tmp rela.rela_symbols);
        R.sh_info = Int64.of_int (section_num tmp rela.rela_target);
        R.sh_entsize = 12L;
        R.sh_addralign = 8L;
      }
    | SHT_REL64 rela ->
      { sh_header with
        R.sh_type = R.SHT_REL;
        R.sh_link = Int64.of_int (section_num tmp rela.rela_symbols);
        R.sh_info = Int64.of_int (section_num tmp rela.rela_target);
        R.sh_entsize = 16L;
        R.sh_addralign = 8L;
      }
    | SHT_RELA64 rela ->
      { sh_header with
        R.sh_type = R.SHT_RELA;
        R.sh_link = Int64.of_int (section_num tmp rela.rela_symbols);
        R.sh_info = Int64.of_int (section_num tmp rela.rela_target);
        R.sh_entsize = 24L;
        R.sh_addralign = 8L;
      }

    | SHT_VERNEED _ -> assert false (* TODO *)
    | SHT_VERSYM _ -> assert false (* TODO *)
  in
  let section = {
    sh_num; sh_content; sh_header; sh_pos = 0;
  } in
  tmp.tmp_pos <- tmp.tmp_pos + String.length sh_content;
  tmp.tmp_sections <- IntMap.add section.sh_num section tmp.tmp_sections;
  tmp.tmp_contents <- sh_content :: tmp.tmp_contents

let string_table tab =
  SHT_STRTAB (Buffer.contents tab.tmpstr_buf)

let int_of_file_class = function
  | ELFCLASSNONE -> 0
  | ELFCLASS32 -> 1
  | ELFCLASS64 -> 2
  | ELFCLASSNUM n -> n

let int_of_data_encoding = function
  | ELFDATANONE -> 0
  | ELFDATA2LSB -> 1
  | ELFDATA2MSB -> 2
  | ELFDATANUM n -> n

let int_of_osabi = function
  |  ELFOSABI_NONE -> 0
  |  ELFOSABI_SYSV -> 0
  |  ELFOSABI_HPUX -> 1
  |  ELFOSABI_NETBSD -> 2
  |  ELFOSABI_LINUX -> 3
  |  ELFOSABI_SOLARIS -> 6
  |  ELFOSABI_AIX -> 7
  |  ELFOSABI_IRIX -> 8
  |  ELFOSABI_FREEBSD -> 9
  |  ELFOSABI_TRU64 -> 10
  |  ELFOSABI_MODESTO -> 11
  |  ELFOSABI_OPENBSD -> 12
  |  ELFOSABI_OPENVMS -> 13
  |  ELFOSABI_NSK -> 14
  |  ELFOSABI_AROS -> 15
  |  ELFOSABI_ARM -> 97
  |  ELFOSABI_STANDALONE -> 255
  |  ELFOSABI_NUM n -> n

let int_of_type = function
  | ET_NONE -> 0
  | ET_REL -> 1
  | ET_EXEC -> 2
  | ET_DYN -> 3
  | ET_CORE -> 4
  | ET_PROC n -> n + 0xff00
  | ET_OS n -> n + 0xfe00
  | ET_NUM n -> n

let int_of_machine = function
  |  EM_NONE ->  0
  |  EM_M32 ->  1
  |  EM_SPARC ->  2
  |  EM_386 ->  3
  |  EM_68K ->  4
  |  EM_88K ->  5
  |  EM_486 ->  6
  |  EM_860 ->  7
  |  EM_MIPS ->  8
  |  EM_S370 ->  9
  |  EM_MIPS_RS3_LE ->  10
  |  EM_SPARC64 ->  11
  |  EM_PARISC ->  15
  |  EM_VPP500 ->  17
  |  EM_SPARC32PLUS ->  18
  |  EM_960 ->  19
  |  EM_PPC ->  20
  |  EM_PPC64 ->  21
  |  EM_S390 ->  22
  |  EM_SPU ->  23
  |  EM_V800 ->  36
  |  EM_FR20 ->  37
  |  EM_RH32 ->  38
  |  EM_RCE ->  39
  |  EM_ARM ->  40
  |  EM_ALPHA ->  41
  |  EM_SH ->  42
  |  EM_SPARCV9 ->  43
  |  EM_TRICORE ->  44
  |  EM_ARC ->  45
  |  EM_H8_300 ->  46
  |  EM_H8_300H ->  47
  |  EM_H8S ->  48
  |  EM_H8_500 ->  49
  |  EM_IA_64 ->  50
  |  EM_MIPS_X ->  51
  |  EM_COLDFIRE ->  52
  |  EM_68HC12 ->  53
  |  EM_MMA ->  54
  |  EM_PCP ->  55
  |  EM_NCPU ->  56
  |  EM_NDR1 ->  57
  |  EM_STARCORE ->  58
  |  EM_ME16 ->  59
  |  EM_ST100 ->  60
  |  EM_TINYJ ->  61
  |  EM_X86_64 ->  62
  |  EM_PDSP ->  63
  |  EM_FX66 ->  66
  |  EM_ST9PLUS ->  67
  |  EM_ST7 ->  68
  |  EM_68HC16 ->  69
  |  EM_68HC11 ->  70
  |  EM_68HC08 ->  71
  |  EM_68HC05 ->  72
  |  EM_SVX ->  73
  |  EM_ST19 ->  74
  |  EM_VAX ->  75
  |  EM_CRIS ->  76
  |  EM_JAVELIN ->  77
  |  EM_FIREPATH ->  78
  |  EM_ZSP ->  79
  |  EM_MMIX ->  80
  |  EM_HUANY ->  81
  |  EM_PRISM ->  82
  |  EM_AVR ->  83
  |  EM_FR30 ->  84
  |  EM_D10V ->  85
  |  EM_D30V ->  86
  |  EM_V850 ->  87
  |  EM_M32R ->  88
  |  EM_MN10300 ->  89
  |  EM_MN10200 ->  90
  |  EM_PJ ->  91
  |  EM_OPENRISC ->  92
  |  EM_ARC_A5 ->  93
  |  EM_XTENSA ->  94
  |  EM_VIDEOCORE ->  95
  |  EM_TMM_GPP ->  96
  |  EM_NS32K ->  97
  |  EM_TPC ->  98
  |  EM_SNP1K ->  99
  |  EM_ST200 ->  100
  |  EM_IP2K ->  101
  |  EM_MAX ->  102
  |  EM_CR ->  103
  |  EM_F2MC16 ->  104
  |  EM_MSP430 ->  105
  |  EM_BLACKFIN ->  106
  |  EM_SE_C33 ->  107
  |  EM_SEP ->  108
  |  EM_ARCA ->  109
  |  EM_UNICORE ->  110
  |  EM_NUM n ->  n

let buf_header tmp =
  let h = tmp.tmp_elf in
  let b = tmp.tmp_buf in
  let en = tmp.tmp_en in
  Buffer.add_string b "\127ELF";
  Buffer.add_char b (char_of_int (int_of_file_class h.e_file_class));
  Buffer.add_char b (char_of_int (int_of_data_encoding h.e_data_encoding));
  Buffer.add_char b (char_of_int h.e_file_version);
  Buffer.add_char b (char_of_int (int_of_osabi h.e_osabi));
  Buffer.add_char b (char_of_int h.e_abi_version);
  for _i = 9 to 15 do
    Buffer.add_char b '\000'
  done;

  let e_shoff = tmp.tmp_pos in
  let e_ehsize = if en.word_size = ARCH32 then 52 else 64 in
  let e_phentsize = 0 in
  let e_phnum = 0 in
  let e_shentsize = if en.word_size = ARCH32 then 40 else 64 in
  let e_shnum = tmp.tmp_nsections in

  buf_half en b (int_of_type h.e_type);
  buf_half en b (int_of_machine h.e_machine);
  buf_word32 en b h.e_version;
  buf_addr_by_class en b h.e_entry;
  buf_off_by_class en b 0L;
  buf_off_by_class en b (Int64.of_int e_shoff);
  buf_word32 en b h.e_flags;
  buf_half en b e_ehsize;
  buf_half en b e_phentsize;
  buf_half en b e_phnum;
  buf_half en b e_shentsize;
  buf_half en b e_shnum;
  buf_half en b (section_num tmp ".shstrtab");
  ()

(*
  let flags = match s_type with
  | SHT_PROGBITS bits | SHT_NOBITS bits -> bits.sht_flags

  | SHT_UNKNOWN sec -> sec.R.section_header.R.sh_flags
  | _ -> []
  in
*)

let int_of_section_type = function
  | R.SHT_NULL -> 0L
  | R.SHT_PROGBITS -> 1L
  | R.SHT_SYMTAB -> 2L
  | R.SHT_STRTAB -> 3L
  | R.SHT_RELA -> 4L
  | R.SHT_HASH -> 5L
  | R.SHT_DYNAMIC -> 6L
  | R.SHT_NOTE -> 7L
  | R.SHT_NOBITS -> 8L
  | R.SHT_REL -> 9L
  | R.SHT_SHLIB -> 10L
  | R.SHT_DYNSYM -> 11L
  | R.SHT_GNU_verdef -> 0x6ffffffdL
  | R.SHT_GNU_verneed -> 0x6ffffffeL
  | R.SHT_GNU_versym -> 0x6fffffffL
  | _ -> assert false

let int_of_section_header_flags flags =
  List.fold_left (fun acc flag ->
    match flag with
    | SHF_WRITE -> acc lor 1
    | SHF_ALLOC -> acc lor 2
    | SHF_EXECINSTR -> acc lor 4
    | SHF_NUM n -> acc lor n
  ) 0 flags

let buf_section_header tmp sh =
  let b = tmp.tmp_buf in
  let en = tmp.tmp_en in
  buf_word32 en b sh.R.sh_name;
  buf_word32 en b (int_of_section_type sh.R.sh_type);
  buf_word_by_class en b (Int64.of_int (int_of_section_header_flags sh.R.sh_flags));
  buf_addr_by_class en b sh.R.sh_addr;
  buf_off_by_class en b sh.R.sh_offset;
  buf_word_by_class en b sh.R.sh_size;
  buf_word32 en b sh.R.sh_link;
  buf_word32 en b sh.R.sh_info;
  buf_word_by_class en b sh.R.sh_addralign;
  buf_word_by_class en b sh.R.sh_entsize;
  ()

let buf_section tmp s =
  if !debug then
    Printf.eprintf "Buffering %d -> %d\n%!"
      s.sh_num (Buffer.length tmp.tmp_buf);
  Buffer.add_string tmp.tmp_buf s.sh_content

let to_string e =

  let en = get_encoding e.e_data_encoding e.e_file_class in

  let buf = Buffer.create 10000 in
  let tmp = {
    tmp_elf = e;
    tmp_en = en;
    tmp_buf = buf;
    tmp_pos = if en.word_size = ARCH32 then 52 else 64;
    tmp_nsections = 0;
    tmp_sections = IntMap.empty;
    tmp_contents = [];
    tmp_map = StringMap.empty;
    tmp_strtab = new_strtab ();
    tmp_shstrtab = new_strtab ();
  } in

  add_section tmp "" SHT_NULL;
  StringMap.iter (fun name s ->
    match name with
    | ""
    | ".strtab"
    | ".shstrtab" -> () (* automatically generated *)
    | _ ->
      add_section tmp name s.s_desc) e.e_sections;
  add_section tmp ".strtab" (string_table tmp.tmp_strtab);

  (* Must be done before saving the content ! *)
  let _ = get_string tmp.tmp_shstrtab  ".shstrtab" in
  add_section tmp ".shstrtab" (string_table tmp.tmp_shstrtab);

  buf_header tmp;
  (*
    List.iter (fun content ->
    Buffer.add_string tmp.tmp_buf content)
    (List.rev tmp.tmp_contents);
  *)
  (*
    for i = 0 to tmp.tmp_nsections - 1 do
    buf_section tmp (IntMap.find i tmp.tmp_sections)
    done; *)
  for i = 0 to tmp.tmp_nsections - 1 do
    let b = tmp.tmp_buf in
    let s = IntMap.find i tmp.tmp_sections in
    s.sh_pos <- Buffer.length b;
    Buffer.add_string b s.sh_content
  done;
  for i = 0 to tmp.tmp_nsections - 1 do
    let s = IntMap.find i tmp.tmp_sections in
    buf_section_header tmp { s.sh_header with
      R.sh_offset = Int64.of_int s.sh_pos }
  done;

  Buffer.contents buf

let to_file filename t =
  let s = to_string t in
  let oc = open_out_bin filename in
  output_string oc s;
  close_out oc
