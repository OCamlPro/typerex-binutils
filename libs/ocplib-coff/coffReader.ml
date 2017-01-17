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
open CoffTypes.RAW
open CoffPrinter

let debug = try ignore (Sys.getenv "OCP_COFF" : string); true with _ -> false

let rec str_spc s pos len =
  if len = 0 then ""
  else
  if s.[pos+len-1] = ' ' then
    str_spc s pos (len-1)
  else
    String.sub s pos len

let get_string0 s pos1 =
  try
    let pos2 = String.index_from s pos1 '\000' in
    String.sub s pos1 (pos2 - pos1), pos2
  with Not_found ->
    let len = String.length s in
    String.sub s pos1 (len - pos1), len

let get_stringN s pos1 =
  try
    let pos2 = String.index_from s pos1 '\n' in
    String.sub s pos1 (pos2 - pos1), pos2
  with Not_found ->
    let len = String.length s in
    String.sub s pos1 (len - pos1), len

let ltl_get_uint32 s pos =
  let v, pos = LittleEndian.get_uint32_64 s pos in
  Int64.to_int v, pos

let ltl_get_uint16 s pos =
  let v, pos = LittleEndian.get_uint16_64 s pos in
  Int64.to_int v, pos

let ltl_get_uint8 s pos =
  let v, pos = LittleEndian.get_uint8_64 s pos in
  Int64.to_int v, pos

module CoffHeader = struct

  exception BadImageFile of int

  let string_of_subsystem = function
    | IMAGE_SUBSYSTEM_UNKNOWN -> "IMAGE_SUBSYSTEM_UNKNOWN"
    | IMAGE_SUBSYSTEM_NATIVE -> "IMAGE_SUBSYSTEM_NATIVE"
    | IMAGE_SUBSYSTEM_WINDOWS_GUI -> "IMAGE_SUBSYSTEM_WINDOWS_GUI"
    | IMAGE_SUBSYSTEM_WINDOWS_CUI -> "IMAGE_SUBSYSTEM_WINDOWS_CUI"
    | IMAGE_SUBSYSTEM_POSIX_CUI -> "IMAGE_SUBSYSTEM_POSIX_CUI"
    | IMAGE_SUBSYSTEM_WINDOWS_CE_GUI -> "IMAGE_SUBSYSTEM_WINDOWS_CE_GUI"
    | IMAGE_SUBSYSTEM_EFI_APPLICATION -> "IMAGE_SUBSYSTEM_EFI_APPLICATION"
    | IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER ->
      "IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER"
    | IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER -> "IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER"
    | IMAGE_SUBSYSTEM_EFI_ROM -> "IMAGE_SUBSYSTEM_EFI_ROM"
    | IMAGE_SUBSYSTEM_XBOX -> "IMAGE_SUBSYSTEM_XBOX"

  let characteristics_of_int n =
    let l = [] in
    let l = if n land 1 = 0 then l else IMAGE_FILE_RELOCS_STRIPPED :: l in
    let l = if n land 2 = 0 then l else IMAGE_FILE_EXECUTABLE_IMAGE :: l in
    let l = if n land 4 = 0 then l else IMAGE_FILE_LINE_NUMS_STRIPPED :: l in
    let l = if n land 8 = 0 then l else IMAGE_FILE_LOCAL_SYMS_STRIPPED :: l in
    let l = if n land 0x10 = 0 then l else IMAGE_FILE_AGGRESIVE_WS_TRIM :: l in
    let l = if n land 0x20 = 0 then l else
        IMAGE_FILE_LARGE_ADDRESS_AWARE :: l in
    let l = if n land 0x80 = 0 then l else IMAGE_FILE_BYTES_REVERSED_LO :: l in
    let l = if n land 0x100 = 0 then l else IMAGE_FILE_32BIT_MACHINE :: l in
    let l = if n land 0x200 = 0 then l else IMAGE_FILE_DEBUG_STRIPPED :: l in
    let l = if n land 0x400 = 0 then l else
        IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP :: l in
    let l = if n land 0x800 = 0 then l else
        IMAGE_FILE_NET_RUN_FROM_SWAP :: l in
    let l = if n land 0x1000 = 0 then l else IMAGE_FILE_SYSTEM :: l in
    let l = if n land 0x2000 = 0 then l else IMAGE_FILE_DLL :: l in
    let l = if n land 0x4000 = 0 then l else IMAGE_FILE_UP_SYSTEM_ONLY :: l in
    if n land 0x8000 = 0 then l else IMAGE_FILE_BYTES_REVERSED_HI :: l

  let int_of_characteristics l =
    List.fold_left (fun acc -> function
      | IMAGE_FILE_RELOCS_STRIPPED -> 0x0001 lor acc
      | IMAGE_FILE_EXECUTABLE_IMAGE ->       0x0002 lor acc
      | IMAGE_FILE_LINE_NUMS_STRIPPED ->     0x0004 lor acc
      | IMAGE_FILE_LOCAL_SYMS_STRIPPED ->    0x0008 lor acc
      | IMAGE_FILE_AGGRESIVE_WS_TRIM ->      0x0010 lor acc
      | IMAGE_FILE_LARGE_ADDRESS_AWARE ->    0x0020 lor acc
      | IMAGE_FILE_BYTES_REVERSED_LO ->      0x0080 lor acc
      | IMAGE_FILE_32BIT_MACHINE ->          0x0100 lor acc
      | IMAGE_FILE_DEBUG_STRIPPED ->         0x0200 lor acc
      | IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP ->0x0400 lor acc
      | IMAGE_FILE_NET_RUN_FROM_SWAP ->      0x0800 lor acc
      | IMAGE_FILE_SYSTEM ->                 0x1000 lor acc
      | IMAGE_FILE_DLL ->                    0x2000 lor acc
      | IMAGE_FILE_UP_SYSTEM_ONLY ->         0x4000 lor acc
      | IMAGE_FILE_BYTES_REVERSED_HI ->      0x8000 lor acc
    ) 0 l

  let machine_of_int = function
    | 0x0 -> IMAGE_FILE_MACHINE_UNKNOWN
    | 0x1d3 -> IMAGE_FILE_MACHINE_AM33
    | 0x8664 -> IMAGE_FILE_MACHINE_AMD64
    | 0x1c0 -> IMAGE_FILE_MACHINE_ARM
    | 0xebc -> IMAGE_FILE_MACHINE_EBC
    | 0x14c -> IMAGE_FILE_MACHINE_I386
    | 0x200 -> IMAGE_FILE_MACHINE_IA64
    | 0x9041 -> IMAGE_FILE_MACHINE_M32R
    | 0x266 -> IMAGE_FILE_MACHINE_MIPS16
    | 0x366 -> IMAGE_FILE_MACHINE_MIPSFPU
    | 0x466 -> IMAGE_FILE_MACHINE_MIPSFPU16
    | 0x1f0 -> IMAGE_FILE_MACHINE_POWERPC
    | 0x1f1 -> IMAGE_FILE_MACHINE_POWERPCFP
    | 0x166 -> IMAGE_FILE_MACHINE_R4000
    | 0x1a2 -> IMAGE_FILE_MACHINE_SH3
    | 0x1a3 -> IMAGE_FILE_MACHINE_SH3DSP
    | 0x1a6 -> IMAGE_FILE_MACHINE_SH4
    | 0x1a8 -> IMAGE_FILE_MACHINE_SH5
    | 0x1c2 -> IMAGE_FILE_MACHINE_THUMB
    | 0x169 -> IMAGE_FILE_MACHINE_WCEMIPSV2
    | n -> raise (BadImageFile n)

  let int_of_machine = function
    | IMAGE_FILE_MACHINE_UNKNOWN -> 0x0
    | IMAGE_FILE_MACHINE_AM33 -> 0x1d3
    | IMAGE_FILE_MACHINE_AMD64 -> 0x8664
    | IMAGE_FILE_MACHINE_ARM -> 0x1c0
    | IMAGE_FILE_MACHINE_EBC -> 0xebc
    | IMAGE_FILE_MACHINE_I386 -> 0x14c
    | IMAGE_FILE_MACHINE_IA64 -> 0x200
    | IMAGE_FILE_MACHINE_M32R -> 0x9041
    | IMAGE_FILE_MACHINE_MIPS16 -> 0x266
    | IMAGE_FILE_MACHINE_MIPSFPU -> 0x366
    | IMAGE_FILE_MACHINE_MIPSFPU16 -> 0x466
    | IMAGE_FILE_MACHINE_POWERPC -> 0x1f0
    | IMAGE_FILE_MACHINE_POWERPCFP -> 0x1f1
    | IMAGE_FILE_MACHINE_R4000 -> 0x166
    | IMAGE_FILE_MACHINE_SH3 -> 0x1a2
    | IMAGE_FILE_MACHINE_SH3DSP -> 0x1a3
    | IMAGE_FILE_MACHINE_SH4  -> 0x1a6
    | IMAGE_FILE_MACHINE_SH5   -> 0x1a8
    | IMAGE_FILE_MACHINE_THUMB  -> 0x1c2
    | IMAGE_FILE_MACHINE_WCEMIPSV2 -> 0x169

  let imageExportTable = 0
  let imageImportTable = 1
  let imageResourceTable = 2
  let imageExceptionTable = 3
  let imageCertificateTable = 4
  let imageBaseRelocationTable = 5
  let imageDebug = 6
  let imageArchitecture = 7
  let imageGlobalPtr = 8
  let imageTLSTable = 9
  let imageLoadConfigTable = 10
  let imageBoundImport = 11
  let imageIAT = 12
  let imageDelayImportDescriptor = 13
  let imageCLRRuntimeHeader = 14
  let imageZERO = 15

  let get_header ic =
    if debug then
      Printf.eprintf "CoffHeader\n%!";
    let s =
      let s = Bytes.create 20 in
      really_input ic s 0 20;
      Bytes.to_string s
    in
    let machine, pos = ltl_get_uint16 s 0 in
    let numberOfSections, pos = ltl_get_uint16 s pos in
    let timeDateStamp, pos = LittleEndian.get_uint32_64 s pos in
    let pointerToSymbolTable, pos = ltl_get_uint32 s pos in
    let numberOfSymbols, pos = ltl_get_uint32 s pos in
    let sizeOfOptionalHeader, pos = ltl_get_uint16 s pos in
    let characteristics, pos = ltl_get_uint16 s pos in
    let machine = machine_of_int machine in
(*    Printf.eprintf "machine = %S\n%!" (string_of_machine machine); *)
    let characteristics = characteristics_of_int characteristics in
(*    Printf.eprintf "characteristics= %s\n%!"
      (String.concat " | " (List.map string_of_characteristics
           characteristics)); *)
    let optionalHeader =
      if sizeOfOptionalHeader > 0 then begin
        let s =
          let s = Bytes.create 2 in
          really_input ic s 0 2;
          Bytes.to_string s
        in
        let pe_magic, _ = ltl_get_uint16 s 0 in
(*        Printf.eprintf "pe_magic = %x\n%!" pe_magic; *)
        let pe_kind = match pe_magic with
          | 0x10b -> PE32
          | 0x20b -> PE64
          | _ -> assert false (* TODO *)
        in
        let get_addr, opt_hdr_len = match pe_kind with
          | PE32 -> LittleEndian.get_uint32_64, 94
          | PE64 -> LittleEndian.get_uint64, 110
        in
        let s =
          let s = Bytes.create opt_hdr_len in
          really_input ic s 0 opt_hdr_len;
          Bytes.to_string s in
        let majorLinkerVersion, pos = ltl_get_uint8 s 0 in
        let minorLinkerVersion, pos = ltl_get_uint8 s pos in
        let sizeOfCode, pos = ltl_get_uint32 s pos in
        let sizeOfInitializedData, pos = ltl_get_uint32 s pos in
        let sizeOfUninitializedData, pos = ltl_get_uint32 s pos in
        let addressOfEntryPoint, pos = ltl_get_uint32 s pos in
        let baseOfCode, pos = ltl_get_uint32 s pos in
        assert (pos = 22);
        let baseOfData, pos = match pe_kind with
          | PE32 -> ltl_get_uint32 s pos
          | PE64 -> 0, pos in
        let imageBase, pos = get_addr s pos in
        assert (pos = 30);
        let sectionAlignment, pos = ltl_get_uint32 s pos in
        let fileAlignment, pos = ltl_get_uint32 s pos in
        let majorOperatingSystemVersion, pos = ltl_get_uint16 s pos in
        let minorOperatingSystemVersion, pos = ltl_get_uint16 s pos in
        let majorImageVersion, pos = ltl_get_uint16 s pos in
        let minorImageVersion, pos = ltl_get_uint16 s pos in
        let majorSubsystemVersion, pos = ltl_get_uint16 s pos in
        let minorSubsystemVersion, pos = ltl_get_uint16 s pos in
        let win32VersionValue, pos = ltl_get_uint32 s pos in
        let sizeOfImage, pos = ltl_get_uint32 s pos in
        assert (pos = 58);
        let sizeOfHeaders, pos = ltl_get_uint32 s pos in
        let checkSum = String.sub s 62 4 in
        let pos = pos + 4 in
        let subsystem, pos = ltl_get_uint16 s pos in
        let dllCharacteristics, pos = ltl_get_uint16 s pos in
        let sizeOfStackReserve, pos = get_addr s pos in
        let sizeOfStackCommit, pos = get_addr s pos in
        let sizeOfHeapReserve, pos = get_addr s pos in
        let sizeOfHeapCommit, pos = get_addr s pos in
        let loaderFlags, pos = ltl_get_uint32 s pos in
        let numberOfRvaAndSizes, pos = ltl_get_uint32 s pos in
        assert (pos = opt_hdr_len);
        let len_directories = numberOfRvaAndSizes * 8 in
        let s=
          let s = Bytes.create len_directories in
          really_input ic s 0 len_directories;
          Bytes.to_string s
        in
        let directories = Array.init numberOfRvaAndSizes
            (fun i ->
              let pos = i*8 in
              let image_rva, pos = LittleEndian.get_uint32_64 s pos in
              let image_size, pos = LittleEndian.get_uint32_64 s pos in
              { image_rva; image_size }
            ) in
        Some {
          pe_kind;
          majorLinkerVersion;
          minorLinkerVersion;
          sizeOfCode;
          sizeOfInitializedData;
          sizeOfUninitializedData;
          addressOfEntryPoint;
          baseOfCode;
          baseOfData;
          imageBase;
          sectionAlignment;
          fileAlignment;
          majorOperatingSystemVersion;
          minorOperatingSystemVersion;
          majorImageVersion;
          minorImageVersion;
          majorSubsystemVersion;
          minorSubsystemVersion;
          win32VersionValue;
          sizeOfImage;
          sizeOfHeaders;
          checkSum;
          subsystem;
          dllCharacteristics;
          sizeOfStackReserve;
          sizeOfStackCommit;
          sizeOfHeapReserve;
          sizeOfHeapCommit;
          loaderFlags;
          numberOfRvaAndSizes;
          directories;
        }
      end else None
    in
    { machine;
      numberOfSections;
      timeDateStamp;
      pointerToSymbolTable;
      numberOfSymbols;
      sizeOfOptionalHeader;
      characteristics;
      optionalHeader;
    }

(* Done. Next section: 13 Section Table *)

end

module SymbolTable = struct

  let get ic obj_base h strtab =
    if debug then
      Printf.eprintf "SymbolTable\n%!";
    let orig_pos = pos_in ic in
    let pos = h.pointerToSymbolTable in
    seek_in ic (obj_base + pos);
    let s = Bytes.create 18 in
    let symbols = Array.init h.numberOfSymbols (fun i ->
      really_input ic s 0 18;
      let s = Bytes.to_string s in
      let sym_name = String.sub s 0 8 in
      let sym_name, _ =
        if sym_name.[0] = '\000' &&
          sym_name.[1] = '\000' &&
          sym_name.[2] = '\000' &&
          sym_name.[3] = '\000'  then
          let pos, _ = ltl_get_uint32 sym_name 4 in
          get_string0 strtab pos
        else get_string0 s 0
      in
      let sym_value, pos = LittleEndian.get_uint32_64 s 8 in
      let sym_sectionNumber, pos = ltl_get_uint16 s pos in
      let sym_type, pos = ltl_get_uint16 s pos in
      let sym_storageClass, pos = ltl_get_uint8 s pos in
      let sym_numberOfAuxSymbols, pos = ltl_get_uint8 s pos in
      {
        sym_name; sym_value;
        sym_sectionNumber; sym_type;
        sym_storageClass; sym_numberOfAuxSymbols;
      }
    ) in
    seek_in ic orig_pos;
    symbols

end

module StringTable = struct

  let get ic obj_base h =
    if debug then
      Printf.eprintf "StringTable\n%!";
    if h.numberOfSymbols = 0 then "" else
      let orig_pos = pos_in ic in
      let pos = h.pointerToSymbolTable + 18 * h.numberOfSymbols in
      if debug then
        Printf.eprintf "pos = %d (%d)\n" pos (obj_base + pos);
      seek_in ic (obj_base + pos);
      let s =
        let s = Bytes.create 4 in
        really_input ic s 0 4;
        Bytes.to_string s
      in
      let len, _pos = ltl_get_uint32 s 0 in
      let len = len - 4 in
      let s =
        let s = Bytes.create len in
        really_input ic s 0 len;
        Bytes.to_string s
      in
      seek_in ic orig_pos;
      s

end

module SectionTable = struct

  let check_bit n flag bit l =
    if n land bit = 0 then l else flag :: l

  let section_flags_of_int nL =
    let n = Int64.to_int nL in
    let l = [] in
    let l = check_bit n IMAGE_SCN_TYPE_NO_PAD 0x00000008 l in
    let l = check_bit n IMAGE_SCN_CNT_CODE 0x00000020 l in
    let l = check_bit n IMAGE_SCN_CNT_INITIALIZED_DATA 0x00000040 l in
    let l = check_bit n IMAGE_SCN_CNT_UNINITIALIZED_DATA 0x00000080 l in
    let l = check_bit n IMAGE_SCN_LNK_OTHER 0x00000100 l in
    let l = check_bit n IMAGE_SCN_LNK_INFO 0x00000200 l in
    let l = check_bit n IMAGE_SCN_LNK_REMOVE 0x00000800 l in
    let l = check_bit n IMAGE_SCN_LNK_COMDAT 0x00001000 l in
    let l = check_bit n IMAGE_SCN_NO_DEFER_SPEC_EXC 0x00004000 l in
    let l = check_bit n IMAGE_SCN_GPREL 0x00008000 l in
    let l = check_bit n IMAGE_SCN_MEM_FARDATA 0x00008000 l in
    let l = check_bit n IMAGE_SCN_MEM_PURGEABLE 0x00020000 l in
    let l = check_bit n IMAGE_SCN_MEM_16BIT 0x00020000 l in
    let l = check_bit n IMAGE_SCN_MEM_LOCKED 0x00040000 l in
    let l = check_bit n IMAGE_SCN_MEM_PRELOAD 0x00080000 l in
    let l = check_bit n IMAGE_SCN_ALIGN_1BYTES 0x00100000 l in
    let l = check_bit n IMAGE_SCN_ALIGN_2BYTES 0x00200000 l in
    let l = check_bit n IMAGE_SCN_ALIGN_4BYTES 0x00300000 l in
    let l = check_bit n IMAGE_SCN_ALIGN_8BYTES 0x00400000 l in
    let l = check_bit n IMAGE_SCN_ALIGN_16BYTES 0x00500000 l in
    let l = check_bit n IMAGE_SCN_ALIGN_32BYTES 0x00600000 l in
    let l = check_bit n IMAGE_SCN_ALIGN_64BYTES 0x00700000 l in
    let l = check_bit n IMAGE_SCN_ALIGN_128BYTES 0x00800000 l in
    let l = check_bit n IMAGE_SCN_ALIGN_256BYTES 0x00900000 l in
    let l = check_bit n IMAGE_SCN_ALIGN_512BYTES 0x00A00000 l in
    let l = check_bit n IMAGE_SCN_ALIGN_1024BYTES 0x00B00000 l in
    let l = check_bit n IMAGE_SCN_ALIGN_2048BYTES 0x00C00000 l in
    let l = check_bit n IMAGE_SCN_ALIGN_4096BYTES 0x00D00000 l in
    let l = check_bit n IMAGE_SCN_ALIGN_8192BYTES 0x00E00000 l in
    let l = check_bit n IMAGE_SCN_LNK_NRELOC_OVFL 0x01000000 l in
    let l = check_bit n IMAGE_SCN_MEM_DISCARDABLE 0x02000000 l in
    let l = check_bit n IMAGE_SCN_MEM_NOT_CACHED 0x04000000 l in
    let l = check_bit n IMAGE_SCN_MEM_NOT_PAGED 0x08000000 l in
    let l = check_bit n IMAGE_SCN_MEM_SHARED 0x10000000 l in
    let l = check_bit n IMAGE_SCN_MEM_EXECUTE 0x20000000 l in
    let l = check_bit n IMAGE_SCN_MEM_READ 0x40000000 l in

    let n = Int64.to_int (Int64.shift_right nL 16) in
    let l = check_bit n IMAGE_SCN_MEM_WRITE 0x8000 l in
    l

    let get ic h strtab =
    if debug then
      Printf.eprintf "SectionTable\n%!";
      let n = h.numberOfSections in
      let s = Bytes.create 40 in
      Array.init n (fun i ->
        let s =
          really_input ic s 0 40;
          Bytes.to_string s in
        let section_name = String.sub s 0 8 in
        let section_name, _ = get_string0 section_name 0 in
        let len = String.length section_name in
        let section_name = if len > 0 && section_name.[0] = '/' then
            let pos = int_of_string (String.sub section_name 1 (len-1)) in
            let s, _ = get_string0 strtab pos in
            s
          else section_name
        (* either:
           * 8 characters
           * '/' + NNN+ '\000' (offset in StringTable)
           * < 8 characters + '\000'

           * .text$X contributes to .text, before .text$Y
        *)
        in
        let virtualSize, pos = LittleEndian.get_uint32_64 s 8 in
        let virtualAddress, pos = LittleEndian.get_uint32_64 s pos in
        let sizeOfRawData, pos = LittleEndian.get_uint32_64 s pos in
        let pointerToRawData, pos = LittleEndian.get_uint32_64 s pos in
        let pointerToRelocations, pos = LittleEndian.get_uint32_64 s pos in
        let pointerToLinenumbers, pos = LittleEndian.get_uint32_64 s pos in
        let numberOfRelocations, pos = ltl_get_uint16 s pos in
        let numberOfLinenumbers, pos = ltl_get_uint16 s pos in
        let sectionFlags, pos = LittleEndian.get_uint32_64 s pos in
        let sectionFlags = section_flags_of_int sectionFlags in
        { section_name;
          virtualSize; virtualAddress;
          sizeOfRawData; pointerToRawData; pointerToRelocations;
          pointerToLinenumbers; numberOfRelocations; numberOfLinenumbers;
          sectionFlags;
        }
      )
end

module Object = struct

  (* [ic] must point to the beginning of the COFF header *)
  let get ic obj_base obj_size =
    let obj_coff_header = CoffHeader.get_header ic in
    let obj_strtab = StringTable.get ic obj_base obj_coff_header in
    let obj_sections = SectionTable.get ic obj_coff_header obj_strtab in
    let obj_symbols = SymbolTable.get ic obj_base obj_coff_header obj_strtab in
    {
      obj_base;
      obj_size;
      obj_coff_header;
      obj_sections;
      obj_symbols;
      obj_strtab;
    }

  let read filename =
    let ic = open_in filename in
    let obj_size = in_channel_length ic in
    get ic 0 obj_size

end

module PEFile = struct

  (* Taken from an existing file... *)
  let stub = "MZ\144\000\003\000\000\000\004\000\000\000\255\255\000\000\184\000\000\000\000\000\000\000@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\216\000\000\000\014\031\186\014\000\180\t\205!\184\001L\205!This program cannot be run in DOS mode.\r\r\n$\000\000\000\000\000\000\000_\184\193\152\027\217\175\203\027\217\175\203\027\217\175\203\152\197\161\203\026\217\175\203\243\198\171\203\025\217\175\203y\198\188\203\030\217\175\203\027\217\174\2033\217\175\203\243\198\165\203\n\217\175\203Rich\027\217\175\203\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

  let read filename =
    let ic = open_in filename in
    let pe_size = in_channel_length ic in
    let s = Bytes.create 4 in
    begin
      really_input ic s 0 4;
      let s = Bytes.to_string s in
      assert (s.[0] = 'M' && s.[1] = 'Z');
    end;
    let pos = 0x3c in
    seek_in ic pos;
    let pe_pos, pos =
      really_input ic s 0 4;
      let s = Bytes.to_string s in
      ltl_get_uint32 s 0 in
(*    Printf.eprintf "pe_pos= %d\n%!" pe_pos; *)
    seek_in ic pe_pos;
    really_input ic s 0 4;
    let s = Bytes.to_string s in
    assert (s = "PE\000\000");
    let pe_obj = Object.get ic 0 pe_size in
    let pe_stub =
      seek_in ic 0;
      let s = Bytes.create pe_pos in
      really_input ic s 0 pe_pos;
      Bytes.to_string s
    in
    {
      pe_content = ic;
      pe_size;
      pe_stub;
      pe_pos;
      pe_obj;
    }

end


module Library = struct

  exception FileIsNotALibrary of string
  exception InvalidLibraryFormat of string

  type member_kind =
      MberLinker of bool
    | MberStringTable of string
    | MberObject of string

  type member = {
    mber_pos : int;

    mber_name : string;
    mber_date : string;
    mber_user : string;
    mber_group : string;
    mber_mode : string;
    mber_size : int;
    mber_kind : member_kind;
  }

(*
name/
   The name of the archive member.
/
  The archive member is one of the two linker members. Both
  of the linker members have this name.
//
  The archive member is the longnames member, which
  consists of a series of null-terminated ASCII strings. The
  longnames member is the third archive member and must
  always be present even if the contents are empty.
/n
  The name of the archive member is located at offset n within
  the longnames member. The number n is the decimal
  representation of the offset. For example: “/26” indicates that
  the name of the archive member is located 26 bytes beyond
  the beginning of the longnames member contents.
*)


  type library = {
    lib_name : string;
    lib_ic : in_channel;
    lib_members : member array;
  }

  let magic = "!<arch>\n"

  let read_member lib m =
    Printf.eprintf "read_member size=%d at pos=%d\n%!"
      m.mber_size (m.mber_pos+60);
    let s = Bytes.create m.mber_size in
    seek_in lib.lib_ic (m.mber_pos+60);
    really_input lib.lib_ic s 0 m.mber_size;
    Bytes.to_string s


(* Symbols sorted in the order of objects files in the archive *)
  let read_linker1 lib =
    let m = lib.lib_members.(0) in
    match m.mber_kind with
    | MberLinker true ->
      let s = read_member lib m in
      let map =       let map = ref IntMap.empty in
        Array.iter (fun m ->
          map := IntMap.add m.mber_pos m !map
        ) lib.lib_members;
        !map in
      let pos = 0 in
      let nsymbolsL, pos = BigEndian.get_uint32_64 s pos in
      let nsymbols = Int64.to_int nsymbolsL in
      let symbol2member = Array.init nsymbols (fun i ->
          let pos = 4 + i*4 in
          let memberL, _pos = BigEndian.get_uint32_64 s pos in
          IntMap.find (Int64.to_int memberL) map
        ) in
      let pos = ref (4 + 4 * nsymbols) in
      let symbols = Array.init nsymbols (fun i ->
          let s, next_pos = get_string0 s !pos in
          pos := next_pos;
          s, symbol2member.(i)
        ) in
      symbols
    | _ -> assert false

  let string_of_kind = function
      MberLinker true -> "linker1"
    | MberLinker false -> "linker2"
    | MberStringTable _ -> "strtab"
    | MberObject name -> Printf.sprintf "object %S" name

(* Symbols sorted in lexicographical order *)
  let read_linker2 lib =
    let m = lib.lib_members.(1) in
    match m.mber_kind with
    | MberLinker false ->
      let s = read_member lib m in
      let map =       let map = ref IntMap.empty in
        Array.iter (fun m ->
          map := IntMap.add m.mber_pos m !map
        ) lib.lib_members;
        !map in
      let pos = 0 in
      let nmembersL, pos = LittleEndian.get_uint32_64 s pos in
      let nmembers = Int64.to_int nmembersL in
      let members = Array.init nmembers (fun i ->
          let pos = 4 + i*4 in
          let memberL, _pos = LittleEndian.get_uint32_64 s pos in
          IntMap.find (Int64.to_int memberL) map
        )
      in
      let pos = 4 + 4 * nmembers in
      let nsymbolsL, pos = LittleEndian.get_uint32_64 s pos in
      let nsymbols = Int64.to_int nsymbolsL in
      let symbol2member = Array.init nsymbols (fun i ->
          let pos = pos + i*2 in
          let memberL, _pos = LittleEndian.get_uint16_64 s pos in
          members.(Int64.to_int memberL - 1)
        ) in
      let pos = ref (pos + 4 * nsymbols) in
      let symbols = Array.init nsymbols (fun i ->
          let s, next_pos = get_string0 s !pos in
          pos := next_pos;
          s, symbol2member.(i)
        ) in
      Some symbols
    | _ ->
      Printf.eprintf "Warning: expecting linker2 member in %S, found %S\n%!"
        lib.lib_name (string_of_kind m.mber_kind);
      None

  let iter_objects f lib =
    Array.iteri (fun i m ->
      match m.mber_kind with
      | MberObject name ->
        seek_in lib.lib_ic  (m.mber_pos+60);
        let obj = Object.get lib.lib_ic (m.mber_pos+60) m.mber_size in
        f lib i obj
      | _ -> ()
    ) lib.lib_members

  let read filename =
    let ic = open_in_bin filename in
    begin
      let s = Bytes.create 8 in
      really_input ic s 0 8;
      if Bytes.to_string s <> magic then begin
        close_in ic;
        raise (FileIsNotALibrary filename)
      end;
    end;
    let first_member = ref true in
    let members = ref [] in
    begin
    try
      let header = Bytes.create 60 in
      let strtbl = ref "" in
      while true do
        let mber_pos = pos_in ic in
        begin try
          really_input ic header 0 60
        with End_of_file -> raise Exit
        end;
        let header = Bytes.to_string header in
(*        Printf.eprintf "header= %S\n%!" header; *)
        let mber_name = str_spc header 0 16 in
        let mber_date = str_spc header 16 12 in
        let mber_user = str_spc header 28 6 in
        let mber_group = str_spc header 34 6 in
        let mber_mode = str_spc header 40 8 in
        let mber_size = str_spc header 48 10 in
        let mber_size = int_of_string mber_size in
        let endHeader = str_spc header 58 2 in
        if (endHeader <> "`\n") then begin
          close_in ic;
          raise (InvalidLibraryFormat filename)
        end;
        let next_pos = mber_pos + mber_size + 60 in
        let next_pos = if next_pos land 1 = 1 then next_pos+1 else next_pos in

        let mber_kind =
          match mber_name with
          | "/" | "" -> MberLinker !first_member
          (* linker member (first or second) *)
          | "//" -> (* string table (third member *)
            let s =
              let s = Bytes.create mber_size in
              really_input ic s 0 mber_size;
              Bytes.to_string s
            in
            strtbl := s;
            MberStringTable s
          | _ ->
            let mber_name =
              if mber_name.[0] = '/' then
                let mber_name_len = String.length mber_name in
                let pos = int_of_string (
                    String.sub mber_name 1 (mber_name_len - 1)) in
                let mber_name = fst (get_string0 !strtbl pos) in
                let mber_name_len = String.length mber_name in
                String.sub mber_name 0 (mber_name_len - 1)
              else
                let mber_name_len = String.length mber_name in
                String.sub mber_name 0 (mber_name_len - 1)
            in
            MberObject mber_name

        in
        first_member := false;
        members := {
          mber_pos;
          mber_name;
          mber_date;
          mber_user;
          mber_group;
          mber_mode;
          mber_size;
          mber_kind;
        } :: !members;

        seek_in ic next_pos;
      done
    with Exit -> ()
    end;
    {
      lib_name = filename;
      lib_ic = ic;
      lib_members = Array.of_list (List.rev !members)
    }

end
