(*The attribute form governs how the value of the attribute is encoded.*)
(*See Figure 21. Attribute form encodings pg 174-175 and
 * Section 7.5.4 Attribute Encodings pg 146 *)

type form_data =
  | OFS_I32 of int32
  | OFS_I64 of int64
  | Flag of bool
  | Block1 of int * char list
  | Block2 of int * char list
  | Block4 of int32 * char list
  | Block of int64 * char list
  | Data1 of char
  | Data2 of int
  | Data4 of int32
  | Data8 of int64
  | String of string
  | Sdata of int64
  | Udata of int64
  | Exprloc of int64 * char list
  | FlagPresent
  | Ref1 of char
  | Ref2 of int
  | Ref4 of int32
  | Ref8 of int64
  | Ref_udata of int64
  | Indirect of int64
