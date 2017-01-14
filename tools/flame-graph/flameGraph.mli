

type tree

type bts = (string list * float) list

type palette =
| Hot
| Mem
| IO

type rgb = int * int * int

val set_title : tree -> string -> unit
val read_folded_file : string -> bts
val tree_of_bts : bts -> tree
val height_of_tree : tree -> int
val width_of_tree : tree -> float


val palette : palette -> string -> rgb

module type DisplayArg = sig

  type t

  val create : title:string -> width:float -> height:float -> t

  val rectangle :
    t ->
    title:string ->
    caption:string ->
    x:float ->
    y:float ->
    width:float ->
    red:int ->
    green:int ->
    blue:int ->
    unit

  val to_string : t -> string

  val box_height : float

end

module type DisplayResult = sig
  val of_tree : (string -> rgb) -> tree -> string
end

module Display(S: DisplayArg) : DisplayResult
module SVG : DisplayResult
