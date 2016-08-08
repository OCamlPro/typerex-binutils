
module Library : sig

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

  type library = {
    lib_name : string;
    lib_ic : in_channel;
    lib_members : member array;
  }

  val read: string -> library

  val string_of_kind: member_kind -> string
  val read_linker1: library -> (string * member) array
  val read_linker2: library -> (string * member) array option
  val iter_objects:
    (library -> int -> CoffTypes.RAW.object_file -> unit) ->
    library -> unit
end

module PEFile : sig
  val stub : string
  val read : string -> in_channel CoffTypes.RAW.pe_file
end

module Object : sig
  val read: string -> CoffTypes.RAW.object_file
end
