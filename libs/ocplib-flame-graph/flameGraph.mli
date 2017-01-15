

type tree

type bts = (string list * float) list

type palette =
| Hot
| Mem
| IO

type rgb = int * int * int

type config = {
  mutable max_depth : int;
  mutable width : int;
  mutable palette : string -> rgb;
  mutable js : string option;
}

val new_config : unit -> config

val new_tree : string -> tree
val enter_bt : tree -> string list -> float -> unit
val enter_bt_log : tree StringCompat.StringMap.t ref -> tree -> string list -> float -> unit

val set_title : tree -> string -> unit
val read_folded_file : string -> bts
val tree_of_bts : bts -> tree
val height_of_tree : tree -> int
val width_of_tree : tree -> float

val palette : palette -> string -> rgb

module type DisplayArg = sig

  type t

  val create :
    title:string ->
    ?js:string ->
    width:float ->
    height:float ->
    unit ->
    t

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
  val of_tree : ?config:config -> tree -> string
end

module Display(S: DisplayArg) : DisplayResult
module SVG : DisplayResult
