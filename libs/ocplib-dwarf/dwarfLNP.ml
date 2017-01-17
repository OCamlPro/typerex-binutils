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

open DwarfFormat
open DwarfUtils
open DwarfTypes

(*Relevant section : 6.2 pg 108*)

(*Line number program : A series of byte-coded line number information *)
(*instructions representing one compilation unit.*)

(*This module extracts the LNP of a CU and executes it to expand it into *)
(*line number data.*)
(*Output is still raw (only the bytecode).*)

type dwarf_CU_LN_header =
  { header_offset : int;
    unit_length : Word64.t;
    version : int;
    header_len : Word64.t;
    min_inst_len : int;
    max_ops_per_inst : int;
    default_is_stmt : int;
    line_base : int;
    line_range : int;
    opcode_base : int;
    standard_opcode_lengths : int list;
    include_directories : string list;
    file_names : (string * int64 * int64 * int64) list; }

type dwarf_LN_OPS =
    DW_LNS_copy
  | DW_LNS_advance_pc of int * int
  | DW_LNS_advance_line of Word64.t * int
  | DW_LNS_set_file of Word64.t
  | DW_LNS_set_column of Word64.t
  | DW_LNS_negate_stmt
  | DW_LNS_set_basic_block
  | DW_LNS_const_add_pc of int * int
  | DW_LNS_fixed_advance_pc of Word64.t
  | DW_LNS_set_prologue_end
  | DW_LNS_set_epilogue_begin
  | DW_LNS_set_isa of Word64.t
  | DW_LNE_end_sequence
  | DW_LNE_set_address of Word64.t
  | DW_LNE_define_file of string * Word64.t * Word64.t * Word64.t
  | DW_LNE_set_discriminator of Word64.t
  | DW_LNE_user of Word64.t
  | DW_LN_spe_op of int * int * int * int * int

type dwarf_line_number_state =
  { mutable address       : int;
    mutable file          : int;
    mutable op_index      : int;
    mutable line          : int;
    mutable column        : int;
    mutable is_stmt       : int;
    mutable basic_block    : bool;
    mutable end_sequence   : bool;
    mutable prologue_end   : bool;
    mutable epilogue_begin : bool;
    mutable isa           : int;
    mutable discriminator : int; }

let read_line_prog_header s =
  let header_offset = !(s.offset) in
  let get_header_length s = match !(DwarfFormat.format) with
    | DWF_32BITS -> Int64.of_int32 (read_int32 s)
    | DWF_64BITS -> read_int64 s in

  let get_standard_opcode_lengths s op_b =
    let rec helper cnt s =
      if cnt = 0 then []
      else let c = read_int8 s in c :: helper (cnt - 1) s in
    helper op_b s in

  let get_include_directories s =
    let dirs = ref [] in
    let exit = ref true in
    while !exit do
      match DwarfUtils.peek s with
      | Some(0) -> let _ = read_int8 s in exit := false
      | Some(c) -> dirs := (read_null_terminated_string s) :: !dirs
      | None -> exit := false
    done; !dirs in

  let get_file_names s =
    let entries = ref [] in
    let exit = ref true in
    while !exit do
      match DwarfUtils.peek s with
      | Some(0) -> let _ = read_int8 s in exit := false
      | Some(c) ->
        begin
          let path = read_null_terminated_string s in
          let index = read_uleb128 s in
          let time = read_uleb128 s in
          let len = read_uleb128 s in
          entries := (path, index, time, len) :: !entries
        end
      | None -> exit := false
    done; !entries in

  let (dwarf_format, unit_length) = DwarfUtils.get_initial_length s in
  let version = read_int16 s in
  let header_len = get_header_length s in
  let min_inst_len = read_int8 s in
  (*Section 6.2.4 5. p113 DWARF 4*)
  (*For non-VLIW architectures, this field is 1, the op_index register is always 0, and the*)
  (*operation pointer is simply the address register.*)
  (*let max_ops_per_inst = read_int8 s in *)
  let default_is_stmt = read_int8 s in
  let line_base = read_int8 s in
  let line_range = read_int8 s in
  let opcode_base = read_int8 s in
  let standard_opcode_lengths = get_standard_opcode_lengths s (opcode_base-1) in
  let include_directories = get_include_directories s in
  let file_names = get_file_names s in

  { header_offset = header_offset;
    unit_length = unit_length;
    version = version;
    header_len = header_len;
    min_inst_len = min_inst_len;
    max_ops_per_inst = 1;
    default_is_stmt = default_is_stmt;
    line_base = DwarfUtils.uint8_to_int8 line_base;
    line_range = line_range;
    opcode_base = opcode_base;
    standard_opcode_lengths = standard_opcode_lengths;
    include_directories = include_directories;
    file_names = file_names; }

let read_line_prog_stmts s h =

  (*Initial LNP state machine*)
  let blank_state =
    {  address = 0;
       file = 1;
       op_index = 0;
       line = 1;
       column = 0;
       is_stmt = h.default_is_stmt;
       basic_block = false;
       end_sequence = false;
       prologue_end = false;
       epilogue_begin = false;
       isa = 0;
       discriminator = 0;
    } in

  let curr_state = ref blank_state in

  let add_entry_new_state state opc args =
    state.basic_block <- false;
    state.prologue_end <- false;
    state.epilogue_begin <- false;
    state.discriminator <- 0 in

  let read_extended_opcode op s ofs_end =
    let dw_lne_lo_user = 0x80 in
    let dw_lne_hi_user = 0xff in
    match op with
    | 0x01 ->
      (* VM state is reset upon leaving the read_line_prog_stmts by setting the exit flag *)
      !(curr_state).end_sequence <- true;
      add_entry_new_state !curr_state op [];
      DW_LNE_end_sequence
    | 0x02 ->
      let operand = if Arch.address_size == 4
        then Int64.of_int32 @@ read_int32 s
        else read_int64 s in
      !(curr_state).address <- Int64.to_int operand;
      DW_LNE_set_address operand
    | 0x03 -> DW_LNE_define_file ("", read_uleb128 s, read_uleb128 s, read_uleb128 s)
    | 0x04 ->
      let operand = read_uleb128 s in
      !(curr_state).discriminator <- Int64.to_int operand;
      DW_LNE_set_discriminator operand
    | n -> if (n >= dw_lne_lo_user) && (n <= dw_lne_hi_user)
      then DW_LNE_user (Int64.of_int n)
      else Printf.kprintf failwith "unknown DW_LNE opcode %x" n in

  let read_standard_opcode opc s =
    match opc with
    | 0x01 -> DW_LNS_copy
    | 0x02 ->
      let operand = read_uleb128 s in
      let address_addend = (Int64.to_int operand) * h.min_inst_len in
      !curr_state.address <- !curr_state.address + address_addend;
      DW_LNS_advance_pc (address_addend, !curr_state.address)
    | 0x03 ->
      let operand = read_sleb128 s in
      !curr_state.line <- !curr_state.line + (Int64.to_int operand);
      DW_LNS_advance_line (operand, !curr_state.line)
    | 0x04 -> DW_LNS_set_file (read_uleb128 s)
    | 0x05 ->
      let operand = read_uleb128 s in
      !curr_state.column <- Int64.to_int operand;
      DW_LNS_set_column (operand)
    | 0x06 ->
      let _ = !curr_state.is_stmt = if !curr_state.is_stmt == 0 then 1 else 0 in
      DW_LNS_negate_stmt
    | 0x07 ->
      !curr_state.basic_block <- true;
      DW_LNS_set_basic_block
    | 0x08 ->
      let adjusted_opcode = 255 - h.opcode_base in
      let address_addend = ((adjusted_opcode / h.line_range) * h.min_inst_len) in
      !curr_state.address <- !curr_state.address + address_addend;
      DW_LNS_const_add_pc (address_addend, !curr_state.address)
    | 0x09 ->
      let operand = read_int16 s in
      !curr_state.address <- !curr_state.address + operand;
      DW_LNS_fixed_advance_pc (Int64.of_int operand)
    | 0x0a ->
      !curr_state.prologue_end <- true;
      DW_LNS_set_prologue_end
    | 0x0b ->
      !curr_state.epilogue_begin <- true;
      DW_LNS_set_epilogue_begin
    | 0x0c ->
      let operand = read_uleb128 s in
      !curr_state.isa <- Int64.to_int operand;
      DW_LNS_set_isa (operand)
    | n -> Printf.kprintf failwith "unknown DW_LNS opcode %x" n in

  let read_special_opcode state opcode =
    let max_ops_per_inst = h.max_ops_per_inst in
    let adjusted_opcode = opcode - h.opcode_base in
    let operation_advance = adjusted_opcode / h.line_range in
    let address_addend = (h.min_inst_len * ((state.op_index + operation_advance) / max_ops_per_inst)) in
    state.address <- state.address + address_addend;
    state.op_index <- (state.op_index + operation_advance) mod max_ops_per_inst;
    let line_addend = h.line_base + (adjusted_opcode mod h.line_range) in
    state.line <- state.line + line_addend;
    add_entry_new_state !curr_state opcode [line_addend; address_addend; state.op_index];
    DW_LN_spe_op (adjusted_opcode, address_addend, state.address, line_addend, state.line) in

  let exit = ref true in
  let res = ref [] in

  while !exit do
    let curr_offset = !(s.offset) in
    match (read_uint8 s) with
    | 0 ->   let ins_len = read_uleb128 s in
      let ext_opc = read_uint8 s in
      let result = read_extended_opcode ext_opc s (Int64.to_int (ins_len)-1) in
      res := !res @ [(curr_offset, result)];
      (*we read a DW_LNE_end_sequence opcode*)
      begin match ext_opc with 0x01 -> exit := false | _ -> () end
    | c when c > 0 && c < h.opcode_base -> res := !res @ [(curr_offset, read_standard_opcode c s)]
    | c when c >= h.opcode_base -> res := !res @ [(curr_offset, read_special_opcode !curr_state c)]
    | c -> exit := false;
  done;
  !res

let readLNPs s =
  let res = ref [] in
  while DwarfUtils.peek s != None do
    let header = read_line_prog_header s in
    let ln = read_line_prog_stmts s header in
    res := (header, ln) :: !res
  done;
  !res
