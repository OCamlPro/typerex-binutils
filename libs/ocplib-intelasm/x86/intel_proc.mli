module StringSet :
  sig
    type elt = string
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
  end
module StringMap :
  sig
    type key = string
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
module IntSet :
  sig
    type elt = int
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
  end
module IntMap :
  sig
    type key = int
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
type condition =
    O
  | NO
  | B
  | C
  | NAE
  | NB
  | NC
  | AE
  | Z
  | E
  | NZ
  | NE
  | BE
  | NA
  | NBE
  | A
  | S
  | NS
  | P
  | PE
  | NP
  | PO
  | L
  | NGE
  | NL
  | GE
  | LE
  | NG
  | NLE
  | G
type rounding = RoundUp | RoundDown | RoundNearest | RoundTruncate
type reloc_table = PLT | GOTPCREL
type data_size = B8 | B16 | B32 | B64
type constant =
    Const of data_size * int64
  | ConstFloat of string
  | ConstLabel of string * reloc_table option
  | ConstAdd of constant * constant
  | ConstSub of constant * constant
type data_type =
    NO
  | REAL4
  | REAL8
  | REAL10
  | BYTE
  | WORD
  | DWORD
  | QWORD
  | TBYTE
  | OWORD
  | NEAR
  | PROC
type suffix = B | W | L | Q
type float_suffix = FS | FL
type register64 =
    RAX
  | RBX
  | RDI
  | RSI
  | RDX
  | RCX
  | RBP
  | RSP
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | RIP
type register8 =
    AL
  | BL
  | CL
  | DL
  | AH
  | BH
  | CH
  | DH
  | DIL
  | SIL
  | R8B
  | R9B
  | R10B
  | R11B
  | BPL
  | R12B
  | R13B
  | SPL
  | R14B
  | R15B
type register16 =
    AX
  | BX
  | DI
  | SI
  | DX
  | CX
  | SP
  | BP
  | R8W
  | R9W
  | R10W
  | R11W
  | R12W
  | R13W
  | R14W
  | R15W
type register32 = R32 of register64
type registerf = XMM of int | TOS | ST of int
type symbol = string * reloc_table option
type offset = symbol option * int64
type 'reg addr = ('reg * int * 'reg option) option * offset
type mem = M32 of register32 addr | M64 of register64 addr
type arg =
    Imm of data_size * offset
  | Rel of data_size * offset
  | Reg8 of register8
  | Reg16 of register16
  | Reg32 of register32
  | Reg64 of register64
  | Regf of registerf
  | Mem of data_type * mem
type instruction =
    NOP
  | ADD of arg * arg
  | SUB of arg * arg
  | XOR of arg * arg
  | OR of arg * arg
  | AND of arg * arg
  | CMP of arg * arg
  | FSTP of arg
  | FCOMPP
  | FCOMP of arg
  | FLD of arg
  | FNSTSW of arg
  | FNSTCW of arg
  | FLDCW of arg
  | FCHS
  | FABS
  | FADD of arg * arg option
  | FSUB of arg * arg option
  | FMUL of arg * arg option
  | FDIV of arg * arg option
  | FSUBR of arg * arg option
  | FDIVR of arg * arg option
  | FILD of arg
  | FISTP of arg
  | HLT
  | FADDP of arg * arg
  | FSUBP of arg * arg
  | FMULP of arg * arg
  | FDIVP of arg * arg
  | FSUBRP of arg * arg
  | FDIVRP of arg * arg
  | FLD1
  | FPATAN
  | FPTAN
  | FCOS
  | FLDLN2
  | FLDLG2
  | FXCH of arg option
  | FYL2X
  | FSIN
  | FSQRT
  | FLDZ
  | SAR of arg * arg
  | SHR of arg * arg
  | SAL of arg * arg
  | INC of arg
  | DEC of arg
  | IMUL of arg * arg option
  | IDIV of arg
  | PUSH of arg
  | POP of arg
  | MOV of arg * arg
  | MOVZX of arg * arg
  | MOVSX of arg * arg
  | MOVSS of arg * arg
  | MOVSXD of arg * arg
  | MOVSD of arg * arg
  | ADDSD of arg * arg
  | SUBSD of arg * arg
  | MULSD of arg * arg
  | DIVSD of arg * arg
  | SQRTSD of arg * arg
  | ROUNDSD of rounding * arg * arg
  | NEG of arg
  | CVTSS2SD of arg * arg
  | CVTSD2SS of arg * arg
  | CVTSI2SD of arg * arg
  | CVTSD2SI of arg * arg
  | CVTTSD2SI of arg * arg
  | UCOMISD of arg * arg
  | COMISD of arg * arg
  | CALL of arg
  | JMP of arg
  | RET
  | TEST of arg * arg
  | SET of condition * arg
  | J of condition * arg
  | CMOV of condition * arg * arg
  | XORPD of arg * arg
  | ANDPD of arg * arg
  | MOVAPD of arg * arg
  | MOVLPD of arg * arg
  | CDQ
  | LEA of arg * arg
  | CQTO
  | LEAVE
  | XCHG of arg * arg
  | BSWAP of arg
type asm_line =
    Section of string list * string option * string list
  | Global of string
  | Constant of constant * data_size
  | Align of bool * int
  | NewLabel of string * data_type
  | Bytes of string
  | Space of int
  | Comment of string
  | External of string * data_type
  | Set of string * constant
  | End
  | Mode386
  | Model of string
  | Cfi_startproc
  | Cfi_endproc
  | Cfi_adjust_cfa_offset of int
  | File of int * string
  | Loc of int * int
  | Private_extern of string
  | Indirect_symbol of string
  | Type of string * string
  | Size of string * constant
  | Ins of instruction
type asm_program = asm_line list
type system =
    S_macosx
  | S_gnu
  | S_cygwin
  | S_solaris
  | S_win32
  | S_linux_elf
  | S_bsd_elf
  | S_beos
  | S_mingw
  | S_win64
  | S_linux
  | S_mingw64
  | S_unknown
val string_of_datatype : data_type -> string
val system : system
val string_of_string_literal : string -> string
val string_of_symbol : string -> string -> string
val string_of_register64 : register64 -> string
val string_of_register8 : register8 -> string
val string_of_register16 : register16 -> string
val string_of_register32 : register32 -> string
val string_of_registerf : registerf -> string
val string_of_condition : condition -> string
val tab : Buffer.t -> unit
val bprint : Buffer.t -> string -> unit
val arch64 : bool ref
val print_assembler : bool ref
val assembler_passes : (asm_program -> asm_program) list ref
val masm : bool
val binary_content : string option ref
val compile : string -> string -> int
val env_OCAMLASM : string list
val debug_by_diff : bool
val assemble_file : string -> string -> int
val asm_code : asm_line list ref
val directive : asm_line -> unit
val emit : instruction -> unit
val reset_asm_code : unit -> unit
val generate_code :
  out_channel -> (Buffer.t -> bool -> asm_line -> unit) -> unit
val string_of_data_size : data_size -> string
