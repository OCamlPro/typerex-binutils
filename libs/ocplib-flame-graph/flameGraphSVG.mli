
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
