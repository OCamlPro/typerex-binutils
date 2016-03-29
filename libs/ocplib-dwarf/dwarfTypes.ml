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

module Word8 = struct
  type t = int64 (* unsigned int8 ? *)
end

module Word16 = struct
  type t = int64 (* unsigned int16 ? *)
end

module Word32 = struct
  type t = int64 (* unsigned int32 ? *)
end

module Word64 = struct
  type t = int64 (* unsigned int64 ? *)
end

module Int8 = struct
  type t = int64 (* signed int8 ? *)
end

module Int16 = struct
  type t = int64 (* signed int16 ? *)
end

module Int32 = struct
  type t = int64 (* signed int32 ? *)
end

module Int64 = Int64

type dwarf_format =
  DWF_32BITS
  | DWF_64BITS

(* Section 7.21 - Line Number Information *)
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

type dwarf_TAG =
    DW_TAG_array_type
  | DW_TAG_class_type
  | DW_TAG_entry_point
  | DW_TAG_enumeration_type
  | DW_TAG_formal_parameter
  | DW_TAG_imported_declaration
  | DW_TAG_label
  | DW_TAG_lexical_block
  | DW_TAG_member
  | DW_TAG_pointer_type
  | DW_TAG_reference_type
  | DW_TAG_compile_unit
  | DW_TAG_string_type
  | DW_TAG_structure_type
  | DW_TAG_subroutine_type
  | DW_TAG_typedef
  | DW_TAG_union_type
  | DW_TAG_unspecified_parameters
  | DW_TAG_variant
  | DW_TAG_common_block
  | DW_TAG_common_inclusion
  | DW_TAG_inheritance
  | DW_TAG_inlined_subroutine
  | DW_TAG_module
  | DW_TAG_ptr_to_member_type
  | DW_TAG_set_type
  | DW_TAG_subrange_type
  | DW_TAG_with_stmt
  | DW_TAG_access_declaration
  | DW_TAG_base_type
  | DW_TAG_catch_block
  | DW_TAG_const_type
  | DW_TAG_constant
  | DW_TAG_enumerator
  | DW_TAG_file_type
  | DW_TAG_friend
  | DW_TAG_namelist
  | DW_TAG_namelist_item
  | DW_TAG_packed_type
  | DW_TAG_subprogram
  | DW_TAG_template_type_parameter
  | DW_TAG_template_value_parameter
  | DW_TAG_thrown_type
  | DW_TAG_try_block
  | DW_TAG_variant_part
  | DW_TAG_variable
  | DW_TAG_volatile_type
  | DW_TAG_dwarf_procedure
  | DW_TAG_restrict_type
  | DW_TAG_interface_type
  | DW_TAG_namespace
  | DW_TAG_imported_module
  | DW_TAG_unspecified_type
  | DW_TAG_partial_unit
  | DW_TAG_imported_unit
  | DW_TAG_condition
  | DW_TAG_shared_type
  | DW_TAG_user of Word64.t          (*   user extension *)
  (*| DW_TAG_type_unit*)
  (*| DW_TAG_rvalue_reference_type*)
  (*| DW_TAG_template_alias*)

type dwarf_AT  =
    DW_AT_sibling              (*   reference *)
  | DW_AT_location             (*   block, loclistptr *)
  | DW_AT_name                 (*   string *)
  | DW_AT_ordering             (*   constant *)
  | DW_AT_byte_size            (*   block, constant, reference *)
  | DW_AT_bit_offset           (*   block, constant, reference *)
  | DW_AT_bit_size             (*   block, constant, reference *)
  | DW_AT_stmt_list            (*   lineptr *)
  | DW_AT_low_pc               (*   address *)
  | DW_AT_high_pc              (*   address *)
  | DW_AT_language             (*   constant *)
  | DW_AT_discr                (*   reference *)
  | DW_AT_discr_value          (*   constant *)
  | DW_AT_visibility           (*   constant *)
  | DW_AT_import               (*   reference *)
  | DW_AT_string_length        (*   block, loclistptr *)
  | DW_AT_common_reference     (*   reference *)
  | DW_AT_comp_dir             (*   string *)
  | DW_AT_const_value          (*   block, constant, string *)
  | DW_AT_containing_type      (*   reference *)
  | DW_AT_default_value        (*   reference *)
  | DW_AT_inline               (*   constant *)
  | DW_AT_is_optional          (*   flag *)
  | DW_AT_lower_bound          (*   block, constant, reference *)
  | DW_AT_producer             (*   string *)
  | DW_AT_prototyped           (*   flag *)
  | DW_AT_return_addr          (*   block, loclistptr *)
  | DW_AT_start_scope          (*   constant *)
  | DW_AT_bit_stride           (*   constant *)
  | DW_AT_upper_bound          (*   block, constant, reference *)
  | DW_AT_abstract_origin      (*   reference *)
  | DW_AT_accessibility        (*   constant *)
  | DW_AT_address_class        (*   constant *)
  | DW_AT_artificial           (*   flag *)
  | DW_AT_base_types           (*   reference *)
  | DW_AT_calling_convention   (*   constant *)
  | DW_AT_count                (*   block, constant, reference *)
  | DW_AT_data_member_location (*   block, constant, loclistptr *)
  | DW_AT_decl_column          (*   constant *)
  | DW_AT_decl_file            (*   constant *)
  | DW_AT_decl_line            (*   constant *)
  | DW_AT_declaration          (*   flag *)
  | DW_AT_discr_list           (*   block *)
  | DW_AT_encoding             (*   constant *)
  | DW_AT_external             (*   flag *)
  | DW_AT_frame_base           (*   block, loclistptr *)
  | DW_AT_friend               (*   reference *)
  | DW_AT_identifier_case      (*   constant *)
  | DW_AT_macro_info           (*   macptr *)
  | DW_AT_namelist_item        (*   block *)
  | DW_AT_priority             (*   reference *)
  | DW_AT_segment              (*   block, loclistptr *)
  | DW_AT_specification        (*   reference *)
  | DW_AT_static_link          (*   block, loclistptr *)
  | DW_AT_type                 (*   reference *)
  | DW_AT_use_location         (*   block, loclistptr *)
  | DW_AT_variable_parameter   (*   flag *)
  | DW_AT_virtuality           (*   constant *)
  | DW_AT_vtable_elem_location (*   block, loclistptr *)
  | DW_AT_allocated            (*   block, constant, reference *)
  | DW_AT_associated           (*   block, constant, reference *)
  | DW_AT_data_location        (*   block *)
  | DW_AT_byte_stride          (*   block, constant, reference *)
  | DW_AT_entry_pc             (*   address *)
  | DW_AT_use_UTF8             (*   flag *)
  | DW_AT_extension            (*   reference *)
  | DW_AT_ranges               (*   rangelistptr *)
  | DW_AT_trampoline           (*   address, flag, reference, string *)
  | DW_AT_call_column          (*   constant *)
  | DW_AT_call_file            (*   constant *)
  | DW_AT_call_line            (*   constant *)
  | DW_AT_description          (*   string *)
  | DW_AT_binary_scale         (*   constant *)
  | DW_AT_decimal_scale        (*   constant *)
  | DW_AT_small                (*   reference *)
  | DW_AT_decimal_sign         (*   constant *)
  | DW_AT_digit_count          (*   constant *)
  | DW_AT_picture_string       (*   string *)
  | DW_AT_mutable              (*   flag *)
  | DW_AT_threads_scaled       (*   flag *)
  | DW_AT_explicit             (*   flag *)
  | DW_AT_object_pointer       (*   reference *)
  | DW_AT_endianity            (*   constant *)
  | DW_AT_elemental            (*   flag *)
  | DW_AT_pure                 (*   flag *)
  | DW_AT_recursive            (*   flag *)
  | DW_AT_signature            (*   reference *)
  | DW_AT_main_subprogram      (*   flag *)
  | DW_AT_data_bit_offset      (*   constant *)
  | DW_AT_const_expr           (*   flag *)
  | DW_AT_enum_class           (*   flag *)
  | DW_AT_linkage_name         (*   string *)
  | DW_AT_user of Word64.t          (*   user extension *)
  | DW_AT_unk of Word64.t

type dwarf_FORM =
    DW_FORM_addr              (*   address *)
  | DW_FORM_block2 (*   block *)
  | DW_FORM_block4 (*   block *)
  | DW_FORM_data2 (*   constant *)
  | DW_FORM_data4 (*   constant, lineptr, loclistptr, macptr, rangelistptr *)
  | DW_FORM_data8 (*   constant, lineptr, loclistptr, macptr, rangelistptr *)
  | DW_FORM_string (*   string *)
  | DW_FORM_block (*   block *)
  | DW_FORM_block1 (*   block *)
  | DW_FORM_data1 (*   constant *)
  | DW_FORM_flag (*   flag *)
  | DW_FORM_sdata (*   constant *)
  | DW_FORM_strp (*   string *)
  | DW_FORM_udata (*   constant *)
  | DW_FORM_ref_addr            (*   reference *)
  | DW_FORM_ref1                (*   reference *)
  | DW_FORM_ref2                (*   reference *)
  | DW_FORM_ref4                (*   reference *)
  | DW_FORM_ref8                (*   reference *)
  | DW_FORM_ref_udata           (*   reference *)
  | DW_FORM_indirect            (*   (see Section 7.5.3 of DWARF3 specification) *)
  | DW_FORM_sec_offset      (* lineptr, loclistptr, macptr, rangelistptr *)
  | DW_FORM_exprloc         (* exprloc *)
  | DW_FORM_flag_present    (* flag *)
  | DW_FORM_ref_sig8        (* reference *)

type dwarf_ATVAL =
    DW_ATVAL_INT of Int64.t
      | DW_ATVAL_UINT of Word64.t
      | DW_ATVAL_STRING of string
      | DW_ATVAL_BLOB of string
      | DW_ATVAL_BOOL  of bool

type dwarf_abbreviation =
    { abbrev_num : Word64.t;
      abbrev_tag : dwarf_TAG;
      abbrev_has_children : bool;
      abbrev_attributes : (dwarf_AT * dwarf_FORM) list;
    }

type dwarf_DIE =
    { die_id : Word64.t; (* Unique identifier for this entry. *)
      (* Unique identifier of this entry's parent. *)
      die_parent : Word64.t option;
      (* Unique identifiers of this entry's children. *)
      die_children : Word64.t list;
      (* Unique identifier of the left sibling in the DIE tree,
         if one exists. *)
      die_sibling_left  : Word64.t option;
      (* Unique identifier of the right sibling in the DIE tree,
         if one exists. *)
      die_sibling_right : Word64.t option;
      (* Type tag. *)
      die_tag          : dwarf_TAG;
      (* Attribute tag and value pairs. *)
      die_attributes   : (dwarf_AT * dwarf_ATVAL) list;
    }

type dwarf_CU_LN_header =
  {
    header_offset : int;
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
    file_names : (string * int * int * int) list; }

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
      mutable discriminator : int;
    }

(*  Section 7.21 - Macro Information *)
type dwarf_MACINFO =
    DW_MACINFO_define of Word64.t * string     (*  Line number and defined symbol with definition *)
    | DW_MACINFO_undef of  Word64.t * string      (*  Line number and undefined symbol *)
    | DW_MACINFO_start_file of Word64.t * Word64.t (*  Marks start of file with the line where the file was included from and a source file index *)
    | DW_MACINFO_end_file                 (*  Marks end of file *)
    | DW_MACINFO_vendor_ext of Word64.t *  string (*  Implementation defined *)

type dwarf_CFA =
  | DW_CFA_advance_loc of Word8.t
  | DW_CFA_offset of  Word8.t * Word64.t
  | DW_CFA_restore of  Word8.t
  | DW_CFA_nop
  | DW_CFA_set_loc of  Word64.t
  | DW_CFA_advance_loc1 of  Word8.t
  | DW_CFA_advance_loc2 of  Word16.t
  | DW_CFA_advance_loc4 of  Word32.t
  | DW_CFA_offset_extended of  Word64.t * Word64.t
  | DW_CFA_restore_extended of  Word64.t
  | DW_CFA_undefined of  Word64.t
  | DW_CFA_same_value of  Word64.t
  | DW_CFA_register of  Word64.t * Word64.t
  | DW_CFA_remember_state
  | DW_CFA_restore_state
  | DW_CFA_def_cfa of  Word64.t * Word64.t
  | DW_CFA_def_cfa_register of  Word64.t
  | DW_CFA_def_cfa_offset of Word64.t
  | DW_CFA_def_cfa_expression  of string
  | DW_CFA_expression of Word64.t * string
  | DW_CFA_offset_extended_sf of Word64.t * Int64.t
  | DW_CFA_def_cfa_sf of Word64.t * Int64.t
  | DW_CFA_def_cfa_offset_sf of Int64.t
  | DW_CFA_val_offset of Word64.t * Word64.t
  | DW_CFA_val_offset_sf of Word64.t * Int64.t
  | DW_CFA_val_expression of Word64.t * string

type dwarf_CIE =
    { cie_augmentation : string;
      cie_codealignmentfactor : Word64.t;
      cie_dataalignmentfactor   : Int64.t;
      cie_returnaddressregister : Word64.t;
      cie_initialinstructions   : dwarf_CFA list;
    }

type dwarf_FDE =
    { fde_ciepointer : Word64.t;
      fde_InitialLocation : Word64.t;
      fde_AddressRange    : Word64.t;
      fde_Instructions    : dwarf_CFA list;
    }

type dwarf_CIEFDE =
    DW_CIE of dwarf_CIE
  | DW_FDE of dwarf_FDE



type dwarf_OP =
    DW_OP_addr of Word64.t
  | DW_OP_deref
  | DW_OP_const1u of Word8.t
  | DW_OP_const1s of Int8.t
  | DW_OP_const2u of Word16.t
  | DW_OP_const2s of Int16.t
  | DW_OP_const4u of Word32.t
  | DW_OP_const4s of Int32.t
  | DW_OP_const8u of Word64.t
  | DW_OP_const8s of Int64.t
  | DW_OP_constu  of Word64.t
  | DW_OP_consts  of Int64.t
  | DW_OP_dup
  | DW_OP_drop
  | DW_OP_over
  | DW_OP_pick of Word8.t
  | DW_OP_swap
  | DW_OP_rot
  | DW_OP_xderef
  | DW_OP_abs
  | DW_OP_and
  | DW_OP_div
  | DW_OP_minus
  | DW_OP_mod
  | DW_OP_mul
  | DW_OP_neg
  | DW_OP_not
  | DW_OP_or
  | DW_OP_plus
  | DW_OP_plus_uconst of Word64.t
  | DW_OP_shl
  | DW_OP_shr
  | DW_OP_shra
  | DW_OP_xor
  | DW_OP_skip of Int16.t
  | DW_OP_bra of Int16.t
  | DW_OP_eq
  | DW_OP_ge
  | DW_OP_gt
  | DW_OP_le
  | DW_OP_lt
  | DW_OP_ne
  | DW_OP_lit0
  | DW_OP_lit1
  | DW_OP_lit2
  | DW_OP_lit3
  | DW_OP_lit4
  | DW_OP_lit5
  | DW_OP_lit6
  | DW_OP_lit7
  | DW_OP_lit8
  | DW_OP_lit9
  | DW_OP_lit10
  | DW_OP_lit11
  | DW_OP_lit12
  | DW_OP_lit13
  | DW_OP_lit14
  | DW_OP_lit15
  | DW_OP_lit16
  | DW_OP_lit17
  | DW_OP_lit18
  | DW_OP_lit19
  | DW_OP_lit20
  | DW_OP_lit21
  | DW_OP_lit22
  | DW_OP_lit23
  | DW_OP_lit24
  | DW_OP_lit25
  | DW_OP_lit26
  | DW_OP_lit27
  | DW_OP_lit28
  | DW_OP_lit29
  | DW_OP_lit30
  | DW_OP_lit31
  | DW_OP_reg0
  | DW_OP_reg1
  | DW_OP_reg2
  | DW_OP_reg3
  | DW_OP_reg4
  | DW_OP_reg5
  | DW_OP_reg6
  | DW_OP_reg7
  | DW_OP_reg8
  | DW_OP_reg9
  | DW_OP_reg10
  | DW_OP_reg11
  | DW_OP_reg12
  | DW_OP_reg13
  | DW_OP_reg14
  | DW_OP_reg15
  | DW_OP_reg16
  | DW_OP_reg17
  | DW_OP_reg18
  | DW_OP_reg19
  | DW_OP_reg20
  | DW_OP_reg21
  | DW_OP_reg22
  | DW_OP_reg23
  | DW_OP_reg24
  | DW_OP_reg25
  | DW_OP_reg26
  | DW_OP_reg27
  | DW_OP_reg28
  | DW_OP_reg29
  | DW_OP_reg30
  | DW_OP_reg31
  | DW_OP_breg0 of Int64.t
  | DW_OP_breg1 of Int64.t
  | DW_OP_breg2 of Int64.t
  | DW_OP_breg3 of Int64.t
  | DW_OP_breg4 of Int64.t
  | DW_OP_breg5 of Int64.t
  | DW_OP_breg6 of Int64.t
  | DW_OP_breg7 of Int64.t
  | DW_OP_breg8 of Int64.t
  | DW_OP_breg9 of Int64.t
  | DW_OP_breg10 of Int64.t
  | DW_OP_breg11 of Int64.t
  | DW_OP_breg12 of Int64.t
  | DW_OP_breg13 of Int64.t
  | DW_OP_breg14 of Int64.t
  | DW_OP_breg15 of Int64.t
  | DW_OP_breg16 of Int64.t
  | DW_OP_breg17 of Int64.t
  | DW_OP_breg18 of Int64.t
  | DW_OP_breg19 of Int64.t
  | DW_OP_breg20 of Int64.t
  | DW_OP_breg21 of Int64.t
  | DW_OP_breg22 of Int64.t
  | DW_OP_breg23 of Int64.t
  | DW_OP_breg24 of Int64.t
  | DW_OP_breg25 of Int64.t
  | DW_OP_breg26 of Int64.t
  | DW_OP_breg27 of Int64.t
  | DW_OP_breg28 of Int64.t
  | DW_OP_breg29 of Int64.t
  | DW_OP_breg30 of Int64.t
  | DW_OP_breg31 of Int64.t
  | DW_OP_regx of Word64.t
  | DW_OP_fbreg of Int64.t
  | DW_OP_bregx of Word64.t * Int64.t
  | DW_OP_piece of Word64.t
  | DW_OP_deref_size of Word8.t
  | DW_OP_xderef_size of Word8.t
  | DW_OP_nop
  | DW_OP_push_object_address
  | DW_OP_call2 of Word16.t
  | DW_OP_call4 of Word32.t
  | DW_OP_call_ref of Word64.t
  | DW_OP_form_tls_address
  | DW_OP_call_frame_cfa
  | DW_OP_bit_piece of Word64.t * Word64.t

type dwarf_ATE =
    DW_ATE_address
  | DW_ATE_boolean
  | DW_ATE_complex_float
  | DW_ATE_float
  | DW_ATE_signed
  | DW_ATE_signed_char
  | DW_ATE_unsigned
  | DW_ATE_unsigned_char
  | DW_ATE_imaginary_float
  | DW_ATE_packed_decimal
  | DW_ATE_numeric_string
  | DW_ATE_edited
  | DW_ATE_signed_fixed
  | DW_ATE_unsigned_fixed
  | DW_ATE_decimal_float

type dwarf_DS =
    DW_DS_unsigned
  | DW_DS_leading_overpunch
  | DW_DS_trailing_overpunch
  | DW_DS_leading_separate
  | DW_DS_trailing_separate

type dwarf_END =
    DW_END_default
  | DW_END_big
  | DW_END_little

type dwarf_ACCESS =
    DW_ACCESS_public
  | DW_ACCESS_protected
  | DW_ACCESS_private

type dwarf_VIS =
    DW_VIS_local
  | DW_VIS_exported
  | DW_VIS_qualified

type dwarf_VIRTUALITY =
    DW_VIRTUALITY_none
  | DW_VIRTUALITY_virtual
  | DW_VIRTUALITY_pure_virtual

type dwarf_LANG =
    DW_LANG_C89
  | DW_LANG_C
  | DW_LANG_Ada83
  | DW_LANG_C_plus_plus
  | DW_LANG_Cobol74
  | DW_LANG_Cobol85
  | DW_LANG_Fortran77
  | DW_LANG_Fortran90
  | DW_LANG_Pascal83
  | DW_LANG_Modula2
  | DW_LANG_Java
  | DW_LANG_C99
  | DW_LANG_Ada95
  | DW_LANG_Fortran95
  | DW_LANG_PLI
  | DW_LANG_ObjC
  | DW_LANG_ObjC_plus_plus
  | DW_LANG_UPC
  | DW_LANG_D

type dwarf_ID =
    DW_ID_case_sensitive
  | DW_ID_up_case
  | DW_ID_down_case
  | DW_ID_case_insensitive

type dwarf_CC =
    DW_CC_normal
  | DW_CC_program
  | DW_CC_nocall

type dwarf_INL =
    DW_INL_not_inlined
  | DW_INL_inlined
  | DW_INL_declared_not_inlined
  | DW_INL_declared_inlined

type dwarf_ORD =
    DW_ORD_row_major
  | DW_ORD_col_major

type dwarf_DSC =
    DW_DSC_label
  | DW_DSC_range

let dw_ate = function
  | 0x01 -> DW_ATE_address
  | 0x02 -> DW_ATE_boolean
  | 0x03 -> DW_ATE_complex_float
  | 0x04 -> DW_ATE_float
  | 0x05 -> DW_ATE_signed
  | 0x06 -> DW_ATE_signed_char
  | 0x07 -> DW_ATE_unsigned
  | 0x08 -> DW_ATE_unsigned_char
  | 0x09 -> DW_ATE_imaginary_float
  | 0x0a -> DW_ATE_packed_decimal
  | 0x0b -> DW_ATE_numeric_string
  | 0x0c -> DW_ATE_edited
  | 0x0d -> DW_ATE_signed_fixed
  | 0x0e -> DW_ATE_unsigned_fixed
  | 0x0f -> DW_ATE_decimal_float
  | n -> Printf.kprintf failwith "unknown DW_ATE %x" n


let dw_ds = function
  | 0x01 -> DW_DS_unsigned
  | 0x02 -> DW_DS_leading_overpunch
  | 0x03 -> DW_DS_trailing_overpunch
  | 0x04 -> DW_DS_leading_separate
  | 0x05 -> DW_DS_trailing_separate
  | n -> Printf.kprintf failwith "unknown DW_DS %x" n

let dw_end = function
  | 0x00 -> DW_END_default
  | 0x01 -> DW_END_big
  | 0x02 -> DW_END_little
  | n -> Printf.kprintf failwith "unknown DW_END %x" n

let dw_access = function
  | 0x01 -> DW_ACCESS_public
  | 0x02 -> DW_ACCESS_protected
  | 0x03 -> DW_ACCESS_private
  | n -> Printf.kprintf failwith "unknown DW_ACCESS %x" n

let dw_vis = function
  | 0x01 -> DW_VIS_local
  | 0x02 -> DW_VIS_exported
  | 0x03 -> DW_VIS_qualified
  | n -> Printf.kprintf failwith "unknown DW_VIS %x" n

let dw_virtuality = function
  | 0x00 -> DW_VIRTUALITY_none
  | 0x01 -> DW_VIRTUALITY_virtual
  | 0x02 -> DW_VIRTUALITY_pure_virtual
  | n -> Printf.kprintf failwith "unknown DW_VIRTUALITY %x" n

let dw_lang = function
  | 0x0001 -> DW_LANG_C89
  | 0x0002 -> DW_LANG_C
  | 0x0003 -> DW_LANG_Ada83
  | 0x0004 -> DW_LANG_C_plus_plus
  | 0x0005 -> DW_LANG_Cobol74
  | 0x0006 -> DW_LANG_Cobol85
  | 0x0007 -> DW_LANG_Fortran77
  | 0x0008 -> DW_LANG_Fortran90
  | 0x0009 -> DW_LANG_Pascal83
  | 0x000a -> DW_LANG_Modula2
  | 0x000b -> DW_LANG_Java
  | 0x000c -> DW_LANG_C99
  | 0x000d -> DW_LANG_Ada95
  | 0x000e -> DW_LANG_Fortran95
  | 0x000f -> DW_LANG_PLI
  | 0x0010 -> DW_LANG_ObjC
  | 0x0011 -> DW_LANG_ObjC_plus_plus
  | 0x0012 -> DW_LANG_UPC
  | 0x0013 -> DW_LANG_D
  | n -> Printf.kprintf failwith "unknown DW_LANG %x" n

let dw_id = function
  | 0x00 -> DW_ID_case_sensitive
  | 0x01 -> DW_ID_up_case
  | 0x02 -> DW_ID_down_case
  | 0x03 -> DW_ID_case_insensitive
  | n -> Printf.kprintf failwith "unknown DW_ID %x" n

let dw_cc = function
  | 0x01 -> DW_CC_normal
  | 0x02 -> DW_CC_program
  | 0x03 -> DW_CC_nocall
  | n -> Printf.kprintf failwith "unknown DW_CC %x" n

let dw_inl = function
  | 0x00 -> DW_INL_not_inlined
  | 0x01 -> DW_INL_inlined
  | 0x02 -> DW_INL_declared_not_inlined
  | 0x03 -> DW_INL_declared_inlined
  | n -> Printf.kprintf failwith "unknown DW_INL %x" n

let dw_ord = function
  | 0x00 -> DW_ORD_row_major
  | 0x01 -> DW_ORD_col_major
  | n -> Printf.kprintf failwith "unknown DW_ORD %x" n

let dw_dsc = function
  | 0x00 -> DW_DSC_label
  | 0x01 -> DW_DSC_range
  | n -> Printf.kprintf failwith "unknown DW_DSC %x" n

let dw_tag =
  let dw_tag_lo_user = 0x4080 in
  let dw_tag_hi_user = 0xffff in
  function
    | 0x01 -> DW_TAG_array_type
    | 0x02 -> DW_TAG_class_type
    | 0x03 -> DW_TAG_entry_point
    | 0x04 -> DW_TAG_enumeration_type
    | 0x05 -> DW_TAG_formal_parameter
    | 0x08 -> DW_TAG_imported_declaration
    | 0x0a -> DW_TAG_label
    | 0x0b -> DW_TAG_lexical_block
    | 0x0d -> DW_TAG_member
    | 0x0f -> DW_TAG_pointer_type
    | 0x10 -> DW_TAG_reference_type
    | 0x11 -> DW_TAG_compile_unit
    | 0x12 -> DW_TAG_string_type
    | 0x13 -> DW_TAG_structure_type
    | 0x15 -> DW_TAG_subroutine_type
    | 0x16 -> DW_TAG_typedef
    | 0x17 -> DW_TAG_union_type
    | 0x18 -> DW_TAG_unspecified_parameters
    | 0x19 -> DW_TAG_variant
    | 0x1a -> DW_TAG_common_block
    | 0x1b -> DW_TAG_common_inclusion
    | 0x1c -> DW_TAG_inheritance
    | 0x1d -> DW_TAG_inlined_subroutine
    | 0x1e -> DW_TAG_module
    | 0x1f -> DW_TAG_ptr_to_member_type
    | 0x20 -> DW_TAG_set_type
    | 0x21 -> DW_TAG_subrange_type
    | 0x22 -> DW_TAG_with_stmt
    | 0x23 -> DW_TAG_access_declaration
    | 0x24 -> DW_TAG_base_type
    | 0x25 -> DW_TAG_catch_block
    | 0x26 -> DW_TAG_const_type
    | 0x27 -> DW_TAG_constant
    | 0x28 -> DW_TAG_enumerator
    | 0x29 -> DW_TAG_file_type
    | 0x2a -> DW_TAG_friend
    | 0x2b -> DW_TAG_namelist
    | 0x2c -> DW_TAG_namelist_item
    | 0x2d -> DW_TAG_packed_type
    | 0x2e -> DW_TAG_subprogram
    | 0x2f -> DW_TAG_template_type_parameter
    | 0x30 -> DW_TAG_template_value_parameter
    | 0x31 -> DW_TAG_thrown_type
    | 0x32 -> DW_TAG_try_block
    | 0x33 -> DW_TAG_variant_part
    | 0x34 -> DW_TAG_variable
    | 0x35 -> DW_TAG_volatile_type
    | 0x36 -> DW_TAG_dwarf_procedure
    | 0x37 -> DW_TAG_restrict_type
    | 0x38 -> DW_TAG_interface_type
    | 0x39 -> DW_TAG_namespace
    | 0x3a -> DW_TAG_imported_module
    | 0x3b -> DW_TAG_unspecified_type
    | 0x3c -> DW_TAG_partial_unit
    | 0x3d -> DW_TAG_imported_unit
    | 0x3f -> DW_TAG_condition
    | 0x40 -> DW_TAG_shared_type
    | n ->
      if n >= dw_tag_lo_user && n <= dw_tag_hi_user then
        DW_TAG_user (Int64.of_int n)
      else
        Printf.kprintf failwith "unknown DW_TAG %x" n
    (*| 0x41 -> DW_TAG_type_unit*)
    (*| 0x42 -> DW_TAG_rvalue_reference_type*)
    (*| 0x43 -> DW_TAG_template_alias*)

let dw_at =
  let dw_at_lo_user = 0x2000 in
  let dw_at_hi_user = 0x3fff in
  function
    0x01 -> DW_AT_sibling
  | 0x02 -> DW_AT_location
  | 0x03 -> DW_AT_name
  | 0x09 -> DW_AT_ordering
  | 0x0b -> DW_AT_byte_size
  | 0x0c -> DW_AT_bit_offset
  | 0x0d -> DW_AT_bit_size
  | 0x10 -> DW_AT_stmt_list
  | 0x11 -> DW_AT_low_pc
  | 0x12 -> DW_AT_high_pc
  | 0x13 -> DW_AT_language
  | 0x15 -> DW_AT_discr
  | 0x16 -> DW_AT_discr_value
  | 0x17 -> DW_AT_visibility
  | 0x18 -> DW_AT_import
  | 0x19 -> DW_AT_string_length
  | 0x1a -> DW_AT_common_reference
  | 0x1b -> DW_AT_comp_dir
  | 0x1c -> DW_AT_const_value
  | 0x1d -> DW_AT_containing_type
  | 0x1e -> DW_AT_default_value
  | 0x20 -> DW_AT_inline
  | 0x21 -> DW_AT_is_optional
  | 0x22 -> DW_AT_lower_bound
  | 0x25 -> DW_AT_producer
  | 0x27 -> DW_AT_prototyped
  | 0x2a -> DW_AT_return_addr
  | 0x2c -> DW_AT_start_scope
  | 0x2e -> DW_AT_bit_stride
  | 0x2f -> DW_AT_upper_bound
  | 0x31 -> DW_AT_abstract_origin
  | 0x32 -> DW_AT_accessibility
  | 0x33 -> DW_AT_address_class
  | 0x34 -> DW_AT_artificial
  | 0x35 -> DW_AT_base_types
  | 0x36 -> DW_AT_calling_convention
  | 0x37 -> DW_AT_count
  | 0x38 -> DW_AT_data_member_location
  | 0x39 -> DW_AT_decl_column
  | 0x3a -> DW_AT_decl_file
  | 0x3b -> DW_AT_decl_line
  | 0x3c -> DW_AT_declaration
  | 0x3d -> DW_AT_discr_list
  | 0x3e -> DW_AT_encoding
  | 0x3f -> DW_AT_external
  | 0x40 -> DW_AT_frame_base
  | 0x41 -> DW_AT_friend
  | 0x42 -> DW_AT_identifier_case
  | 0x43 -> DW_AT_macro_info
  | 0x44 -> DW_AT_namelist_item
  | 0x45 -> DW_AT_priority
  | 0x46 -> DW_AT_segment
  | 0x47 -> DW_AT_specification
  | 0x48 -> DW_AT_static_link
  | 0x49 -> DW_AT_type
  | 0x4a -> DW_AT_use_location
  | 0x4b -> DW_AT_variable_parameter
  | 0x4c -> DW_AT_virtuality
  | 0x4d -> DW_AT_vtable_elem_location
  | 0x4e -> DW_AT_allocated
  | 0x4f -> DW_AT_associated
  | 0x50 -> DW_AT_data_location
  | 0x51 -> DW_AT_byte_stride
  | 0x52 -> DW_AT_entry_pc
  | 0x53 -> DW_AT_use_UTF8
  | 0x54 -> DW_AT_extension
  | 0x55 -> DW_AT_ranges
  | 0x56 -> DW_AT_trampoline
  | 0x67 -> DW_AT_call_column
  | 0x58 -> DW_AT_call_file
  | 0x59 -> DW_AT_call_line
  | 0x5a -> DW_AT_description
  | 0x5b -> DW_AT_binary_scale
  | 0x5c -> DW_AT_decimal_scale
  | 0x5d -> DW_AT_small
  | 0x5e -> DW_AT_decimal_sign
  | 0x5f -> DW_AT_digit_count
  | 0x60 -> DW_AT_picture_string
  | 0x61 -> DW_AT_mutable
  | 0x62 -> DW_AT_threads_scaled
  | 0x63 -> DW_AT_explicit
  | 0x64 -> DW_AT_object_pointer
  | 0x65 -> DW_AT_endianity
  | 0x66 -> DW_AT_elemental
  (*| 0x67 -> DW_AT_pure*)
  | 0x68 -> DW_AT_recursive
  | 0x69 -> DW_AT_signature
  | 0x6a -> DW_AT_main_subprogram
  | 0x6b -> DW_AT_data_bit_offset
  | 0x6c -> DW_AT_const_expr
  | 0x6d -> DW_AT_enum_class
  | 0x6e -> DW_AT_linkage_name
  | n ->
      if n >= dw_at_lo_user && n <= dw_at_hi_user then
        DW_AT_user (Int64.of_int n)
      else
        DW_AT_unk (Int64.of_int n)

let dw_form =
  function
    0x01 -> DW_FORM_addr
  | 0x03 -> DW_FORM_block2
  | 0x04 -> DW_FORM_block4
  | 0x05 -> DW_FORM_data2
  | 0x06 -> DW_FORM_data4
  | 0x07 -> DW_FORM_data8
  | 0x08 -> DW_FORM_string
  | 0x09 -> DW_FORM_block
  | 0x0a -> DW_FORM_block1
  | 0x0b -> DW_FORM_data1
  | 0x0c -> DW_FORM_flag
  | 0x0d -> DW_FORM_sdata
  | 0x0e -> DW_FORM_strp
  | 0x0f -> DW_FORM_udata
  | 0x10 -> DW_FORM_ref_addr
  | 0x11 -> DW_FORM_ref1
  | 0x12 -> DW_FORM_ref2
  | 0x13 -> DW_FORM_ref4
  | 0x14 -> DW_FORM_ref8
  | 0x15 -> DW_FORM_ref_udata
  | 0x16 -> DW_FORM_indirect
  | 0x17 -> DW_FORM_sec_offset
  | 0x18 -> DW_FORM_exprloc
  | 0x19 -> DW_FORM_flag_present
  | 0x20 -> DW_FORM_ref_sig8
  | n -> Printf.kprintf failwith "unknown DW_FORM %x" n

