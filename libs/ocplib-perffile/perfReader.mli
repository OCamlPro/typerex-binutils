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

(**
  [read ~on_attr ~on_event ~filename] opens file [filename] as a PERFFILE,
  calls [on_attr] on each perf_event_attr value,
  calls [on_event] on each perf_event_type value, and returns the
  number of perf_event_type values read from the file.
*)

val read :
  ?on_attr: (PerfTypes.perf_event_attr -> int64 * int64 -> unit) ->
  on_event:(PerfTypes.perf_event_type -> unit) ->
  filename: string ->
  int (* number of events read *)
