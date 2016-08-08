
module RAW : sig

  val object_file:
    string ->
    ?with_symbols:bool -> CoffTypes.RAW.object_file -> string

  val pe_file:
      string ->
      ?with_symbols:bool -> 'a CoffTypes.RAW.pe_file -> string
end
