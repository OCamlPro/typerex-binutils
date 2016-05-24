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

open Printf

open Zipper
open DwarfTypes
open Form_data
open DwarfUtils
open DwarfAbbrev
open DwarfLNP
open DwarfDIE
open DwarfLocs

let lit =
  function
    | 0x30 -> "DW_OP_lit0"
    | 0x31 -> "DW_OP_lit1"
    | 0x32 -> "DW_OP_lit2"
    | 0x33 -> "DW_OP_lit3"
    | 0x34 -> "DW_OP_lit4"
    | 0x35 -> "DW_OP_lit5"
    | 0x36 -> "DW_OP_lit6"
    | 0x37 -> "DW_OP_lit7"
    | 0x38 -> "DW_OP_lit8"
    | 0x39 -> "DW_OP_lit9"
    | 0x3a -> "DW_OP_lit10"
    | 0x3b -> "DW_OP_lit11"
    | 0x3c -> "DW_OP_lit12"
    | 0x3d -> "DW_OP_lit13"
    | 0x3e -> "DW_OP_lit14"
    | 0x3f -> "DW_OP_lit15"
    | 0x40 -> "DW_OP_lit16"
    | 0x41 -> "DW_OP_lit17"
    | 0x42 -> "DW_OP_lit18"
    | 0x43 -> "DW_OP_lit19"
    | 0x44 -> "DW_OP_lit20"
    | 0x45 -> "DW_OP_lit21"
    | 0x46 -> "DW_OP_lit22"
    | 0x47 -> "DW_OP_lit23"
    | 0x48 -> "DW_OP_lit24"
    | 0x49 -> "DW_OP_lit25"
    | 0x4a -> "DW_OP_lit26"
    | 0x4b -> "DW_OP_lit27"
    | 0x4c -> "DW_OP_lit28"
    | 0x4d -> "DW_OP_lit29"
    | 0x4e -> "DW_OP_lit30"
    | 0x4f -> "DW_OP_lit31"
    | _ -> "unk lit"

let reg n regmap =
  let reg_num = n - 0x50 in
  let s = match n with
    | 0x50 -> "DW_OP_reg0"
    | 0x51 -> "DW_OP_reg1"
    | 0x52 -> "DW_OP_reg2"
    | 0x53 -> "DW_OP_reg3"
    | 0x54 -> "DW_OP_reg4"
    | 0x55 -> "DW_OP_reg5"
    | 0x56 -> "DW_OP_reg6"
    | 0x57 -> "DW_OP_reg7"
    | 0x58 -> "DW_OP_reg8"
    | 0x59 -> "DW_OP_reg9"
    | 0x5a -> "DW_OP_reg10"
    | 0x5b -> "DW_OP_reg11"
    | 0x5c -> "DW_OP_reg12"
    | 0x5d -> "DW_OP_reg13"
    | 0x5e -> "DW_OP_reg14"
    | 0x5f -> "DW_OP_reg15"
    | 0x60 -> "DW_OP_reg16"
    | 0x61 -> "DW_OP_reg17"
    | 0x62 -> "DW_OP_reg18"
    | 0x63 -> "DW_OP_reg19"
    | 0x64 -> "DW_OP_reg20"
    | 0x65 -> "DW_OP_reg21"
    | 0x66 -> "DW_OP_reg22"
    | 0x67 -> "DW_OP_reg23"
    | 0x68 -> "DW_OP_reg24"
    | 0x69 -> "DW_OP_reg25"
    | 0x6a -> "DW_OP_reg26"
    | 0x6b -> "DW_OP_reg27"
    | 0x6c -> "DW_OP_reg28"
    | 0x6d -> "DW_OP_reg29"
    | 0x6e -> "DW_OP_reg30"
    | 0x6f -> "DW_OP_reg31"
    | _ -> "unk reg" in
  sprintf "%s (%s)" s (regmap reg_num)

let breg n ofs regmap =
  let reg_num = n - 0x70 in
  let s = match n with
    | 0x70 -> "DW_OP_breg0"
    | 0x71 -> "DW_OP_breg1"
    | 0x72 -> "DW_OP_breg2"
    | 0x73 -> "DW_OP_breg3"
    | 0x74 -> "DW_OP_breg4"
    | 0x75 -> "DW_OP_breg5"
    | 0x76 -> "DW_OP_breg6"
    | 0x77 -> "DW_OP_breg7"
    | 0x78 -> "DW_OP_breg8"
    | 0x79 -> "DW_OP_breg9"
    | 0x7a -> "DW_OP_breg10"
    | 0x7b -> "DW_OP_breg11"
    | 0x7c -> "DW_OP_breg12"
    | 0x7d -> "DW_OP_breg13"
    | 0x7e -> "DW_OP_breg14"
    | 0x7f -> "DW_OP_breg15"
    | 0x80 -> "DW_OP_breg16"
    | 0x81 -> "DW_OP_breg17"
    | 0x82 -> "DW_OP_breg18"
    | 0x83 -> "DW_OP_breg19"
    | 0x84 -> "DW_OP_breg20"
    | 0x85 -> "DW_OP_breg21"
    | 0x86 -> "DW_OP_breg22"
    | 0x87 -> "DW_OP_breg23"
    | 0x88 -> "DW_OP_breg24"
    | 0x89 -> "DW_OP_breg25"
    | 0x8a -> "DW_OP_breg26"
    | 0x8b -> "DW_OP_breg27"
    | 0x8c -> "DW_OP_breg28"
    | 0x8d -> "DW_OP_breg29"
    | 0x8e -> "DW_OP_breg30"
    | 0x8f -> "DW_OP_breg31"
    | _ -> "nope" in
  sprintf "%s: %d (%s) %Ld" s reg_num (regmap reg_num) ofs

let string_of_op blk reg_map =
  let dw_op_lo_user = 0xe0 in
  let dw_op_hi_user = 0xff in
  match blk with
    (*0 operand operations*)
    | 0x06 :: tl -> (tl, "DW_OP_deref")
    | 0x12 :: tl -> (tl, "DW_OP_dup")
    | 0x13 :: tl -> (tl, "DW_OP_drop")
    | 0x14 :: tl -> (tl, "DW_OP_over")
    | 0x16 :: tl -> (tl, "DW_OP_swap")
    | 0x17 :: tl -> (tl, "DW_OP_rot")
    | 0x18 :: tl -> (tl, "DW_OP_xderef")
    | 0x19 :: tl -> (tl, "DW_OP_abs")
    | 0x1a :: tl -> (tl, "DW_OP_and")
    | 0x1b :: tl -> (tl, "DW_OP_div")
    | 0x1c :: tl -> (tl, "DW_OP_minus")
    | 0x1d :: tl -> (tl, "DW_OP_mod")
    | 0x1e :: tl -> (tl, "DW_OP_mul")
    | 0x1f :: tl -> (tl, "DW_OP_neg")
    | 0x20 :: tl -> (tl, "DW_OP_not")
    | 0x21 :: tl -> (tl, "DW_OP_or")
    | 0x22 :: tl -> (tl, "DW_OP_plus")
    | 0x24 :: tl -> (tl, "DW_OP_shl")
    | 0x25 :: tl -> (tl, "DW_OP_shr")
    | 0x26 :: tl -> (tl, "DW_OP_shra")
    | 0x27 :: tl -> (tl, "DW_OP_xor")
    | 0x29 :: tl -> (tl, "DW_OP_eq")
    | 0x2a :: tl -> (tl, "DW_OP_ge")
    | 0x2b :: tl -> (tl, "DW_OP_gt")
    | 0x2c :: tl -> (tl, "DW_OP_le")
    | 0x2d :: tl -> (tl, "DW_OP_lt")
    | 0x2e :: tl -> (tl, "DW_OP_ne")
    | 0x96 :: tl -> (tl, "DW_OP_nop")
    | 0x97 :: tl -> (tl, "DW_OP_push_object_address")
    | 0x9b :: tl -> (tl, "DW_OP_form_tls_address")
    | 0x9c :: tl -> (tl, "DW_OP_call_frame_cfa")
    | 0x9f :: tl -> (tl, "DW_OP_stack_value")
    (*1 operand operations*)
    | 0x03 :: tl -> begin
      let op = "DW_OP_addr" in
      match Arch.address_size with
        | 4 -> begin let (r, v) = int32_from_list tl in (r, sprintf "%s: %lx" op v) end
        | _ -> begin let (r, v) = int64_from_list tl in (r, sprintf "%s: %Lx" op v) end end
    | 0x08 :: x :: tl -> (tl, sprintf "DW_OP_const1u: %d" x)
    | 0x09 :: x :: tl -> (tl, sprintf "DW_OP_const1s: %d" x)
    | 0x0a :: tl -> let (r, v) = int16_from_list tl in (r, sprintf "DW_OP_const2u: %d" v)
    | 0x0b :: tl -> let (r, v) = int16_from_list tl in (r, sprintf "DW_OP_const2s: %d" v)
    | 0x0c :: tl -> let (r, v) = int32_from_list tl in (r, sprintf "DW_OP_const4u: %ld" v)
    | 0x0d :: tl -> let (r, v) = int32_from_list tl in (r, sprintf "DW_OP_const4s: %ld" v)
    | 0x0e :: tl -> let (r, v) = int64_from_list tl in (r, sprintf "DW_OP_const8u: %Ld" v)
    | 0x0f :: tl -> let (r, v) = int64_from_list tl in (r, sprintf "DW_OP_const8s: %Ld" v)
    | 0x10 :: tl -> let (r, v) = uleb128_from_list tl in (r, sprintf "DW_OP_constu: %Ld" v)
    | 0x11 :: tl -> let (r, v) = sleb128_from_list tl in (r, sprintf "DW_OP_consts: %Ld" v)
    | 0x15 :: x :: tl -> (tl, sprintf "DW_OP_pick (%d)" x)
    | 0x23 :: tl -> let (r, v) = uleb128_from_list tl in (r, sprintf "DW_OP_plus_uconst %Ld" v)
    | 0x28 :: tl -> let (r, v) = int16_from_list tl in (r, sprintf "DW_OP_bra %d" v)
    | 0x2f :: tl -> let (r, v) = int16_from_list tl in (r, sprintf "DW_OP_skip %d" v)
    | 0x90 :: tl -> let (r, v) = uleb128_from_list tl in (r, sprintf "DW_OP_regx: %Ld (%s)" v (reg_map (Int64.to_int v)))
    | 0x91 :: tl -> let (r, v) = sleb128_from_list tl in (r, sprintf "DW_OP_fbreg: %Ld" v)
    | 0x93 :: tl -> let (r, v) = uleb128_from_list tl in (r, sprintf "DW_OP_piece: %Ld" v)
    | 0x94 :: x :: tl -> (tl, sprintf "DW_OP_deref_size (%d)" x)
    | 0x95 :: x :: tl -> (tl, sprintf "DW_OP_xderef_size (%d)" x)
    | 0x98 :: tl -> let (r, v) = int16_from_list tl in (r, sprintf "DW_OP_call2 0x%d" v)
    | 0x99 :: tl -> let (r, v) = int32_from_list tl in (r, sprintf "DW_OP_call4 0x%ld" v)
    | 0x9a :: tl ->
      begin
      let op = "DW_OP_call_ref" in
      match Arch.address_size with
        | 4 -> begin let (r, v) = int32_from_list tl in (r, sprintf "%s : 0%lx" op v) end
        | _ -> begin let (r, v) = int64_from_list tl in (r, sprintf "%s : 0%Lx" op v) end
      end
    (*2 operands operations*)
    | 0x9d :: tl ->
            let (r1, uleb_size) = uleb128_from_list tl in
            let (r2, uleb_ofs) = uleb128_from_list r1 in
            (r2, sprintf "DW_OP_bit_piece (%Ld) : 0x%Lx" uleb_size uleb_ofs)
    | 0x9e :: tl ->
            let rec drop n = function
                | [] -> []
                | h :: t as l -> if n = 0 then l else drop (n-1) t
            in
            let (r1, uleb_size) = uleb128_from_list tl in
            (drop (Int64.to_int uleb_size) r1, sprintf "DW_OP_implicit_value (%Ld) : " uleb_size)
    | 0x92 :: tl ->
            let (r1, uleb_reg) = uleb128_from_list tl in
            let (r2, sleb_ofs) = sleb128_from_list r1 in
            (r2, sprintf "DW_OP_bregx: %Ld (%s) %Lx" uleb_reg (reg_map @@ Int64.to_int uleb_reg) sleb_ofs)

    | [] -> ([], "")
    | n :: tl when n >= 0x30 && n < 0x50 -> (tl, lit n)
    | n :: tl when n >= 0x50 && n < 0x70 -> (tl, reg n reg_map)
    | n :: tl when n >= 0x70 && n < 0x90 ->
            let (rest, ofs) = sleb128_from_list tl in (rest, breg n ofs reg_map)
    | n :: tl ->
      if n >= dw_op_lo_user && n <= dw_op_hi_user then
        (tl, "DW_OP_user")
      else
        (tl, sprintf "unk_op_%d" n)

let string_of_AT =
  function
    DW_AT_sibling ->"DW_AT_sibling"
  | DW_AT_location ->"DW_AT_location"
  | DW_AT_name ->"DW_AT_name"
  | DW_AT_ordering ->"DW_AT_ordering"
  | DW_AT_byte_size ->"DW_AT_byte_size"
  | DW_AT_bit_offset ->"DW_AT_bit_offset"
  | DW_AT_bit_size ->"DW_AT_bit_size"
  | DW_AT_stmt_list ->"DW_AT_stmt_list"
  | DW_AT_low_pc ->"DW_AT_low_pc"
  | DW_AT_high_pc ->"DW_AT_high_pc"
  | DW_AT_language ->"DW_AT_language"
  | DW_AT_discr ->"DW_AT_discr"
  | DW_AT_discr_value ->"DW_AT_discr_value"
  | DW_AT_visibility ->"DW_AT_visibility"
  | DW_AT_import ->"DW_AT_import"
  | DW_AT_string_length ->"DW_AT_string_length"
  | DW_AT_common_reference ->"DW_AT_common_reference"
  | DW_AT_comp_dir ->"DW_AT_comp_dir"
  | DW_AT_const_value ->"DW_AT_const_value"
  | DW_AT_containing_type ->"DW_AT_containing_type"
  | DW_AT_default_value ->"DW_AT_default_value"
  | DW_AT_inline ->"DW_AT_inline"
  | DW_AT_is_optional ->"DW_AT_is_optional"
  | DW_AT_lower_bound ->"DW_AT_lower_bound"
  | DW_AT_producer ->"DW_AT_producer"
  | DW_AT_prototyped ->"DW_AT_prototyped"
  | DW_AT_return_addr ->"DW_AT_return_addr"
  | DW_AT_start_scope ->"DW_AT_start_scope"
  | DW_AT_bit_stride ->"DW_AT_bit_stride"
  | DW_AT_upper_bound ->"DW_AT_upper_bound"
  | DW_AT_abstract_origin ->"DW_AT_abstract_origin"
  | DW_AT_accessibility ->"DW_AT_accessibility"
  | DW_AT_address_class ->"DW_AT_address_class"
  | DW_AT_artificial ->"DW_AT_artificial"
  | DW_AT_base_types ->"DW_AT_base_types"
  | DW_AT_calling_convention ->"DW_AT_calling_convention"
  | DW_AT_count ->"DW_AT_count"
  | DW_AT_data_member_location ->"DW_AT_data_member_location"
  | DW_AT_decl_column ->"DW_AT_decl_column"
  | DW_AT_decl_file ->"DW_AT_decl_file"
  | DW_AT_decl_line ->"DW_AT_decl_line"
  | DW_AT_declaration ->"DW_AT_declaration"
  | DW_AT_discr_list ->"DW_AT_discr_list"
  | DW_AT_encoding ->"DW_AT_encoding"
  | DW_AT_external ->"DW_AT_external"
  | DW_AT_frame_base ->"DW_AT_frame_base"
  | DW_AT_friend ->"DW_AT_friend"
  | DW_AT_identifier_case ->"DW_AT_identifier_case"
  | DW_AT_macro_info ->"DW_AT_macro_info"
  | DW_AT_namelist_item ->"DW_AT_namelist_item"
  | DW_AT_priority ->"DW_AT_priority"
  | DW_AT_segment ->"DW_AT_segment"
  | DW_AT_specification ->"DW_AT_specification"
  | DW_AT_static_link ->"DW_AT_static_link"
  | DW_AT_type ->"DW_AT_type"
  | DW_AT_use_location ->"DW_AT_use_location"
  | DW_AT_variable_parameter ->"DW_AT_variable_parameter"
  | DW_AT_virtuality ->"DW_AT_virtuality"
  | DW_AT_vtable_elem_location ->"DW_AT_vtable_elem_location"
  | DW_AT_allocated ->"DW_AT_allocated"
  | DW_AT_associated ->"DW_AT_associated"
  | DW_AT_data_location ->"DW_AT_data_location"
  | DW_AT_byte_stride ->"DW_AT_byte_stride"
  | DW_AT_entry_pc ->"DW_AT_entry_pc"
  | DW_AT_use_UTF8 ->"DW_AT_use_UTF8"
  | DW_AT_extension ->"DW_AT_extension"
  | DW_AT_ranges ->"DW_AT_ranges"
  | DW_AT_trampoline ->"DW_AT_trampoline"
  | DW_AT_call_column ->"DW_AT_call_column"
  | DW_AT_call_file ->"DW_AT_call_file"
  | DW_AT_call_line ->"DW_AT_call_line"
  | DW_AT_description ->"DW_AT_description"
  | DW_AT_binary_scale ->"DW_AT_binary_scale"
  | DW_AT_decimal_scale ->"DW_AT_decimal_scale"
  | DW_AT_small ->"DW_AT_small"
  | DW_AT_decimal_sign ->"DW_AT_decimal_sign"
  | DW_AT_digit_count ->"DW_AT_digit_count"
  | DW_AT_picture_string ->"DW_AT_picture_string"
  | DW_AT_mutable ->"DW_AT_mutable"
  | DW_AT_threads_scaled ->"DW_AT_threads_scaled"
  | DW_AT_explicit ->"DW_AT_explicit"
  | DW_AT_object_pointer ->"DW_AT_object_pointer"
  | DW_AT_endianity ->"DW_AT_endianity"
  | DW_AT_elemental ->"DW_AT_elemental"
  | DW_AT_pure ->"DW_AT_pure"
  | DW_AT_recursive ->"DW_AT_recursive"
  | DW_AT_signature -> "DW_AT_signature"
  | DW_AT_main_subprogram -> "DW_AT_main_subprogram"
  | DW_AT_data_bit_offset -> "DW_AT_data_bit_offset"
  | DW_AT_const_expr -> "DW_AT_const_expr"
  | DW_AT_enum_class -> "DW_AT_enum_class"
  | DW_AT_linkage_name -> "DW_AT_linkage_name"
  | DW_AT_user w ->"DW_AT_user"
  | DW_AT_unk w -> Printf.sprintf "unknown DW_AT %Lu" w

let string_of_TAG =
  function
    DW_TAG_array_type ->"DW_TAG_array_type"
  | DW_TAG_class_type ->"DW_TAG_class_type"
  | DW_TAG_entry_point ->"DW_TAG_entry_point"
  | DW_TAG_enumeration_type ->"DW_TAG_enumeration_type"
  | DW_TAG_formal_parameter ->"DW_TAG_formal_parameter"
  | DW_TAG_imported_declaration ->"DW_TAG_imported_declaration"
  | DW_TAG_label ->"DW_TAG_label"
  | DW_TAG_lexical_block ->"DW_TAG_lexical_block"
  | DW_TAG_member ->"DW_TAG_member"
  | DW_TAG_pointer_type ->"DW_TAG_pointer_type"
  | DW_TAG_reference_type ->"DW_TAG_reference_type"
  | DW_TAG_compile_unit ->"DW_TAG_compile_unit"
  | DW_TAG_string_type ->"DW_TAG_string_type"
  | DW_TAG_structure_type ->"DW_TAG_structure_type"
  | DW_TAG_subroutine_type ->"DW_TAG_subroutine_type"
  | DW_TAG_typedef ->"DW_TAG_typedef"
  | DW_TAG_union_type ->"DW_TAG_union_type"
  | DW_TAG_unspecified_parameters ->"DW_TAG_unspecified_parameters"
  | DW_TAG_variant ->"DW_TAG_variant"
  | DW_TAG_common_block ->"DW_TAG_common_block"
  | DW_TAG_common_inclusion ->"DW_TAG_common_inclusion"
  | DW_TAG_inheritance ->"DW_TAG_inheritance"
  | DW_TAG_inlined_subroutine ->"DW_TAG_inlined_subroutine"
  | DW_TAG_module ->"DW_TAG_module"
  | DW_TAG_ptr_to_member_type ->"DW_TAG_ptr_to_member_type"
  | DW_TAG_set_type ->"DW_TAG_set_type"
  | DW_TAG_subrange_type ->"DW_TAG_subrange_type"
  | DW_TAG_with_stmt ->"DW_TAG_with_stmt"
  | DW_TAG_access_declaration ->"DW_TAG_access_declaration"
  | DW_TAG_base_type ->"DW_TAG_base_type"
  | DW_TAG_catch_block ->"DW_TAG_catch_block"
  | DW_TAG_const_type ->"DW_TAG_const_type"
  | DW_TAG_constant ->"DW_TAG_constant"
  | DW_TAG_enumerator ->"DW_TAG_enumerator"
  | DW_TAG_file_type ->"DW_TAG_file_type"
  | DW_TAG_friend ->"DW_TAG_friend"
  | DW_TAG_namelist ->"DW_TAG_namelist"
  | DW_TAG_namelist_item ->"DW_TAG_namelist_item"
  | DW_TAG_packed_type ->"DW_TAG_packed_type"
  | DW_TAG_subprogram ->"DW_TAG_subprogram"
  | DW_TAG_template_type_parameter ->"DW_TAG_template_type_parameter"
  | DW_TAG_template_value_parameter ->"DW_TAG_template_value_parameter"
  | DW_TAG_thrown_type ->"DW_TAG_thrown_type"
  | DW_TAG_try_block ->"DW_TAG_try_block"
  | DW_TAG_variant_part ->"DW_TAG_variant_part"
  | DW_TAG_variable ->"DW_TAG_variable"
  | DW_TAG_volatile_type ->"DW_TAG_volatile_type"
  | DW_TAG_dwarf_procedure ->"DW_TAG_dwarf_procedure"
  | DW_TAG_restrict_type ->"DW_TAG_restrict_type"
  | DW_TAG_interface_type ->"DW_TAG_interface_type"
  | DW_TAG_namespace ->"DW_TAG_namespace"
  | DW_TAG_imported_module ->"DW_TAG_imported_module"
  | DW_TAG_unspecified_type ->"DW_TAG_unspecified_type"
  | DW_TAG_partial_unit ->"DW_TAG_partial_unit"
  | DW_TAG_imported_unit ->"DW_TAG_imported_unit"
  | DW_TAG_condition ->"DW_TAG_condition"
  | DW_TAG_shared_type ->"DW_TAG_shared_type"
  | DW_TAG_user _ -> "DW_TAG_user"

let string_of_abbrev_decl d =
  printf "Number TAG\n";
  printf "%Ld %s\n" d.abbrev_num (string_of_TAG d.abbrev_tag);
  if d.abbrev_has_children then printf "[has children]\n" else printf "[no children]\n";
  let _ = List.map (fun (n,f) -> printf "%s %s\n" (string_of_AT n) (Form.string_of_FORM f)) d.abbrev_attributes in ()

let string_of_abbrev_section tbl =
  Hashtbl.iter (fun k v -> string_of_abbrev_decl v; printf "\n") tbl

let string_of_lineprog_header h =
  printf "  Offset: %22s%x\n" "0x" h.header_offset;
  printf "  Length: %23Lu\n" h.unit_length;
  printf "  DWARF Version: %14d\n" h.version;
  printf "  Prologue length: %13Lu\n" h.header_len;

  printf "  Minimum Instruction Length: %d\n" h.min_inst_len;
  (*printf "max_ops_per_inst : %d\n" h.max_ops_per_inst;*)
  printf "  Initial value of 'is_stmt': %d\n" h.default_is_stmt;

  printf "  Line Base: %19d\n" (uint8_to_int8 h.line_base);
  printf "  Line Range: %18d\n" h.line_range;
  printf "  Opcode Base: %17d\n" h.opcode_base;
  print_endline "";

  print_endline " Opcodes:";
  List.iteri (fun i a -> printf "  Opcode %d has %d args\n" (i+1) a) h.standard_opcode_lengths;
  print_endline "";

  if h.include_directories == []
  then print_endline "The Directory Table is empty.\n"
  else begin
      print_endline " The Directory Table:";
      List.iteri (fun i a -> printf "  %d\t%s\n" (i+1) a) h.include_directories;
      print_endline ""
  end;

  if h.file_names == []
  then printf "The File Name Table is empty.\n"
  else begin
      print_endline " The File Name Table:";
      printf "  Entry\tDir\tTime\tSize\tName\n";
      List.iteri (fun i (a,b,c,d) -> printf "  %d\t%Ld\t%Ld\t%Ld\t%s\n" (i+1) b c d a) h.file_names;
      print_endline "";
  end

let string_of_lns_op =
    function
    DW_LNS_copy -> sprintf "DW_LNS_copy \n"
  | DW_LNS_advance_pc (n, adr) -> sprintf "DW_LNS_advance_pc by %d to 0x%x\n" n adr
  | DW_LNS_advance_line (n, l) -> sprintf "DW_LNS_advance_line by %Ld to %d\n" n l
  | DW_LNS_set_file n -> sprintf "DW_LNS_set_file %Lu \n" n
  | DW_LNS_set_column n -> sprintf "DW_LNS_set_column %Lu \n" n
  | DW_LNS_negate_stmt -> sprintf "DW_LNS_negate_stmt \n"
  | DW_LNS_set_basic_block -> sprintf "DW_LNS_set_basic_block \n"
  | DW_LNS_const_add_pc (n, a) -> sprintf "DW_LNS_const_add_pc by %d to 0x%x \n" n a
  | DW_LNS_fixed_advance_pc n -> sprintf "DW_LNS_fixed_advance_pc %Lu \n" n
  | DW_LNS_set_prologue_end -> sprintf "DW_LNS_set_prologue_end \n"
  | DW_LNS_set_epilogue_begin -> sprintf "DW_LNS_set_epilogue_begin \n"
  | DW_LNS_set_isa n -> sprintf "DW_LNS_set_isa %Lu \n" n
  | DW_LNE_end_sequence -> sprintf "DW_LNE_end_sequence \n"
  | DW_LNE_set_address n -> sprintf "DW_LNE_set_address to 0x%Lx \n" n
  | DW_LNE_define_file (s, a, b, c) -> sprintf "DW_LNE_define_file %s %Lu %Lu %Lu \n" s a b c
  | DW_LNE_set_discriminator n -> sprintf "DW_LNE_set_discriminator %Lu \n" n
  | DW_LNE_user n -> sprintf "DW_LNE_user %Lu \n" n
  | DW_LN_spe_op (opc, aa, adr, la, li) -> sprintf "DW_LN_spe_op %d advance address by %d to 0x%x and line by %d to %d\n" opc aa adr la li

let string_of_lineprg l =
    print_endline "Line Number Statements:";
    List.iteri (fun i (ofs,op) -> printf "  [0x%08x]  %s" ofs (string_of_lns_op op)) l;
    print_endline "\n"

let string_of_form_val f s dst =
    let string_of_ofs ofs = read_null_terminated_string {dst with offset = ref ofs} in
    let print_block b =
        let rec h s l = match l with
                    | [] -> s
                    | hd :: tl -> h (s ^ Printf.sprintf "%x " (int_of_char hd)) tl in
        h "" b
        in
    match f with
      (*DW_FORM_addr *)
      | (`address, OFS_I32 (i)) -> Printf.sprintf "%s \t : 0x%lx\n" s i
      | (`address, OFS_I64 (i)) ->  Printf.sprintf "%s \t : 0x%Lx\n" s i
      (*DW_FORM_block1 *)
      | (`block, Block1 (length, b)) -> Printf.sprintf "%s \t Block of length %d : %s\n" s length (print_block b)
      (*DW_FORM_block2 *)
      | (`block, Block2 (length, b)) -> Printf.sprintf "%s \t Block of length %d : %s\n" s length (print_block b)
      (*DW_FORM_block4 *)
      | (`block, Block4 (length, b)) -> Printf.sprintf "%s \t Block of length %ld : %s\n" s length (print_block b)
      (*DW_FORM_block *)
      | (`block, Block (length, b)) -> Printf.sprintf "%s \t Block of length %Ld : %s\n" s length (print_block b)
      (*DW_FORM_data1*)
      | (`constant, Data1 c) -> Printf.sprintf "%s \t : %x\n" s (int_of_char c)
      (*DW_FORM_data2 *)
      | (`constant, Data2 h) -> Printf.sprintf "%s \t : %d\n" s h
     (*DW_FORM_data4 *)
      | (`constant, Data4 (w)) -> Printf.sprintf "%s \t : 0x%lx\n" s w;
      (*DW_FORM_data8 *)
      | (`constant, Data8 (dw)) -> Printf.sprintf "%s \t : %Lx\n" s dw;
      (*DW_FORM_sdata *)
      | (`constant, Sdata (sleb128)) -> Printf.sprintf "%s \t : %Lx\n" s sleb128
      (*DW_FORM_udata *)
      | (`constant, Udata (uleb128)) -> Printf.sprintf "%s \t : %Lx\n" s uleb128
      (*DW_FORM_string *)
      | (`string, String (ss)) -> Printf.sprintf "%s \t : %s\n" s ss
      (*DW_FORM_strp*)
      | (`string, OFS_I32 (i)) ->
        Printf.sprintf "%s : (indirect string, offset: 0x%lx): %s\n" s i (string_of_ofs (Int64.to_int (Int64.of_int32 i)))
      | (`string, OFS_I64 (i)) ->
        Printf.sprintf "%s : (indirect string, offset: 0x%Lx): %s\n" s i (string_of_ofs (Int64.to_int i))
      (*DW_FORM_flag*)
      | (`flag, Flag f) -> let v = if f then "true" else "false" in Printf.sprintf "%s \t flag %s\n" s v
      (*DW_FORM_flag_present *)
      | (`flag, FlagPresent) -> Printf.sprintf "%s \t : 1\n" s
      (*DW_FORM_ref1 *)
      | (`reference, Ref1 c) -> Printf.sprintf "%s \t : %x\n" s (int_of_char c)
      (*DW_FORM_ref2 *)
      | (`reference, Ref2 h) -> Printf.sprintf "%s \t : %x\n" s h
      (*DW_FORM_ref4 *)
      | (`reference, Ref4 w) -> Printf.sprintf "%s \t : %lx\n" s w
      (*DW_FORM_ref8 *)
      | (`reference, Ref8 dw) -> Printf.sprintf "%s \t : %Lx\n" s dw
      (*DW_FORM_ref_udata *)
      | (`reference, Ref_udata uleb128)  -> Printf.sprintf "%s \t : %Lx\n" s uleb128
      (*DW_FORM_ref_sig8 *)
      (*DW_FORM_ref_addr *)
      | (`reference, OFS_I32 i) -> Printf.sprintf "%s \t : %lx\n" s i
      | (`reference, OFS_I64 i) -> Printf.sprintf "%s \t : %Lx\n" s i
      (*DW_FORM_indirect *)
      | (`indirect, Udata uleb128) -> Printf.sprintf "%s \t %Lx\n" s uleb128
      (*DW_FORM_sec_offset *)
      | (`ptr, OFS_I32 i) -> Printf.sprintf "%s \t : %lx\n" s i
      | (`ptr, OFS_I64 i) -> Printf.sprintf "%s \t : %Lx\n" s i
      (*DW_FORM_exprloc *)
      | (`exprloc, Exprloc (length, b)) -> Printf.sprintf "%s \t Block of length %Ld : %s\n" s length (print_block b)
      | (_, _) -> ""

let rec string_of_DIE d debug_str =
  let rec he l1 l2 s =
      match l1, l2 with
      | [], [] -> s
      | (at, _)::tl1, (ofs, fv)::tl2 ->
          he tl1 tl2 (s ^ (Printf.sprintf "    <%x>   " ofs) ^ (string_of_form_val fv (string_of_AT at) debug_str))
      | _, _ -> s in
  begin
  match d.die_cu_header with
  Some(h) ->
      begin
      Printf.printf "  Compilation Unit @ offset 0x%x\n" d.die_ofs;
      Printf.printf "   Length: \t  0x%Lx\n" h.unit_length;
      Printf.printf "   Version: \t  %d\n" h.version;
      Printf.printf "   Abbrev Offset: 0x%Lx\n" h.abbrev_offset;
      Printf.printf "   Pointer Size:  %d\n" h.address_size;
      Printf.printf " <%d><%x>: Abbrev Number: %Lu (%s)\n" d.depth 0 d.abbrev_nu (string_of_TAG d.die_tag);
      end
  | _ -> Printf.printf " <%d><%x>: Abbrev Number: %Lu (%s)\n" d.depth d.die_ofs d.abbrev_nu (string_of_TAG d.die_tag);
  end;
  Printf.printf "%s" (he d.die_attributes d.die_attribute_vals "")

(*"Inspired" from binutils/dwarf.c dwarf_regnames_i386 and dwarf_regnames_x86_64*)

let num_to_reg_i386 =
  function
  | 0  -> "eax"
  | 1  -> "ecx"
  | 2  -> "edx"
  | 3  -> "ebx"
  | 4  -> "esp"
  | 5  -> "ebp"
  | 6  -> "esi"
  | 7  -> "edi"
  | 8  -> "eip"
  | _  -> "unk reg"

let num_to_reg_x86_64 =
  function
  | 0  -> "rax"
  | 1  -> "rdx"
  | 2  -> "rcx"
  | 3  -> "rbx"
  | 4  -> "rsi"
  | 5  -> "rdi"
  | 6  -> "rbp"
  | 7  -> "rsp"
  | 8  -> "r8"
  | 9  -> "r9"
  | 10 -> "r10"
  | 11 -> "r11"
  | 12 -> "r12"
  | 13 -> "r13"
  | 14 -> "r14"
  | 15 -> "r15"
  | 16 -> "rip"
  | _  -> "unk reg"

let rec string_of_locs l =
  let reg_names = match Arch.address_size with
    4 -> num_to_reg_i386
   | _ -> num_to_reg_x86_64 in
  let rec print_expr_block blk res =
      match blk with
      | [] -> res
      | l -> let (r, s) = (string_of_op l reg_names) in print_expr_block r (res ^ s ^ "; ") in
  let print_line loc =
    printf "    %08Lx " loc.entry_offset;
    let expr_str = print_expr_block loc.dwarf_location_description "" in
    match loc.start_offset, loc.end_offset with
      | Ofs32 a, Ofs32 b -> printf "%08lx %08lx (%s)\n" a b expr_str
      | Ofs64 a, Ofs64 b -> printf "%016Lx %016Lx (%s)\n" a b expr_str
      | _, _ -> () in
  match l with
    | [] -> ()
    | [x] -> printf "    %08Lx <End of list>\n" x.entry_offset
    | hd :: tl -> print_line hd; string_of_locs tl

let print_caml_locs l pvm =
  let print_info x tbl =
    let fst = List.hd x in
    let (spn, pvn, base, is_var) = try
        Hashtbl.find tbl fst.entry_offset
    with Not_found -> ("", "", Int64.zero, false) in
    printf "function : %s, %s : %s, base address : %Lx\n" spn (if is_var then "variable" else "parameter") pvn base in

  Hashtbl.iter (fun loc (spn, pvn, sppc, _) ->
    printf "%s %s : %Lx with %Lx\n" spn pvn loc sppc
  ) pvm;

  print_endline "\n    Offset   Begin\t\tEnd\t\tExpression";
  List.iter (fun elt ->
    print_info elt pvm;
    string_of_locs elt) l;
  print_endline ""

let print_locs l =
  print_endline "    Offset   Begin\t\tEnd\t\tExpression";
  List.iter (fun elt ->
    string_of_locs elt) l;
  print_endline ""

let print_LNPs l =
  let rec h = function
      | [] -> ()
      | (hdr, lns) :: tl -> string_of_lineprog_header hdr; string_of_lineprg lns; h tl in
  print_endline "Raw dump of debug contents of section .debug_line:\n";
  h l

let print_abbrevs t =
  Hashtbl.iter (fun k v -> printf "abbrevs for offset 0x%x\n" k;
                string_of_abbrev_section v;
                printf "----------------------\n") t

let print_DIEs l ds =
  print_endline "Contents of the .debug_info section:";
  print_endline "";
  List.iter (fun t ->
      Zipper.fold_tree2 (fun x -> string_of_DIE x ds) (fun x ys -> ()) t
  ) l

let dump_CU_tree fname cu =
  let rec trav t =
    match t with
      | Branch(x, []) -> [sprintf "    h_0x%x;\n" x.die_ofs]
      | Branch(x, cs) ->
          List.map
          (fun c ->
            match c with Branch(cc,_) ->
            sprintf "    h_0x%x -> h_0x%x;\n" x.die_ofs cc.die_ofs
          ) cs
          @ List.concat @@ List.map trav cs in

  let oc = open_out (fname ^ ".dot") in
  fprintf oc "digraph BST {\n";
  fprintf oc "    nodesep=0.4;\n";
  fprintf oc "    ranksep=0.5;\n";
  fprintf oc "    node [fontname=\"Arial\"];\n";

  List.iter (output_string oc) (trav cu);
  fprintf oc "}\n"; close_out oc
