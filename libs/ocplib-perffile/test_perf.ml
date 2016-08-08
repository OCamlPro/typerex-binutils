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

let _ =
  let on_attr attr ids =
    Printf.printf "%Ld [%Ld] : %s\n" (fst ids) (snd ids)
      (PerfPrinter.perf_event_attr attr);
  in
  let on_event ev =
    Printf.printf "%s\n%!" (PerfPrinter.perf_event_type ev);
  in

  let nevents = PerfReader.read
    ~on_attr
    ~on_event
    ~filename: Sys.argv.(1) in

  Printf.printf "nevents = %d\n%!" nevents;
  ()
