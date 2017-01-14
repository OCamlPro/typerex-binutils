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

open StringCompat
open PerfTypes

let verbose = ref 1

let expected_header_size = 104   (* 104 = 8 + 8 + 8 + 16 + 16 + 16 + 32 *)
let event_type_size = 72
let expected_event_attr_size = 72
let perf_event_header_size = 8
type t = unit

let read_perf_file_section s pos =
  let offsetL, pos = LittleEndian.get_int64 s pos in
  let sizeL, pos = LittleEndian.get_int64 s pos in
  (offsetL, sizeL), pos

let is_bit_set v bit = v land (1 lsl bit) <> 0

let read_perf_event_attr event_types s initial_pos =
  let attr_type, pos = LittleEndian.get_int s initial_pos in
  let attr_size, pos = LittleEndian.get_int s pos in
  assert (attr_size >= expected_event_attr_size);
  if !verbose >= 0 && attr_size > expected_event_attr_size then
    Printf.eprintf "Warning: event_attr_size %d > %d, new version ?\n%!"
      attr_size expected_event_attr_size;
  let attr_config, pos = LittleEndian.get_uint64 s pos in
  let attr_config_name = try
                           Some (Int64Map.find attr_config event_types)
    with Not_found -> None in
  let attr_sample, pos = LittleEndian.get_uint64 s pos in
  let attr_sample_type, pos = LittleEndian.get_uint64 s pos in
  let attr_sample_type = Int64.to_int attr_sample_type in
  let attr_read_format, pos = LittleEndian.get_uint64 s pos in
  let attr_read_format = Int64.to_int attr_read_format in
  let attr_attributes,_ = LittleEndian.get_uint64 s pos in
  let c0, pos = LittleEndian.get_uint8 s pos in
  let c12, pos = LittleEndian.get_uint16 s pos in
  let c3, pos = LittleEndian.get_uint s pos in
  let c4567, pos = LittleEndian.get_uint32_64 s pos in
  let attr_disabled = is_bit_set c0 0 in
  let attr_inherit = is_bit_set c0 1 in
  let attr_pinned = is_bit_set c0 2 in
  let attr_exclusive = is_bit_set c0 3 in
  let attr_exclude_user = is_bit_set c0 4 in
  let attr_exclude_kernel = is_bit_set c0 5 in
  let attr_exclude_hv = is_bit_set c0 6 in
  let attr_exclude_idle = is_bit_set c0 7 in

  let attr_mmap = is_bit_set c12 0 in
  let attr_comm = is_bit_set c12 1 in
  let attr_freq = is_bit_set c12 2 in
  let attr_inherit_stat = is_bit_set c12 3 in
  let attr_enable_on_exec = is_bit_set c12 4 in
  let attr_task = is_bit_set c12 5 in
  let attr_watermark = is_bit_set c12 6 in
  let attr_precise_ip = (c12 lsr 7) land 0x3 in
  let attr_mmap_data = is_bit_set c12 9 in
  let attr_sample_id_all = is_bit_set c12 10 in
  let attr_exclude_host = is_bit_set c12 11 in
  let attr_exclude_guest = is_bit_set c12 12 in
  let attr_wakeup, pos = LittleEndian.get_uint32_64 s pos in
  let attr_bp_type, pos = LittleEndian.get_uint32_64 s pos in
  let attr_bp_addr, pos = LittleEndian.get_uint64 s pos in
  let attr_bp_len, pos = LittleEndian.get_uint64 s pos in

  let attr = {
    attr_type = perf_type_id attr_type;
    attr_size;
    attr_config; attr_config_name;
    attr_sample = if attr_freq then AttrSampleFreq attr_sample else AttrSamplePeriod attr_sample;
    attr_sample_type;
    attr_read_format;
    attr_disabled;
    attr_inherit; attr_pinned; attr_exclusive; attr_exclude_user;
    attr_exclude_kernel; attr_exclude_hv; attr_exclude_idle; attr_mmap;
    attr_comm; attr_freq; attr_inherit_stat; attr_enable_on_exec; attr_task;
    attr_watermark; attr_precise_ip; attr_mmap_data; attr_sample_id_all;
    attr_exclude_host; attr_exclude_guest;
    attr_attributes;
    (* attr__reserved_1;  *)
    attr_wakeup = if attr_watermark then AttrWakeUpWatermark attr_wakeup
      else AttrWakeUpEvents attr_wakeup;
    attr_bp_type; attr_bp_addr; attr_bp_len
  } in

  attr, initial_pos + attr_size

let get_rev_uint32_64 s pos =
  let pos = pos - 4 in
  let v, _ = LittleEndian.get_uint32_64 s pos in
  v, pos

let get_rev_uint64 s pos =
  let pos = pos - 8 in
  let v, _ = LittleEndian.get_uint64 s pos in
  v, pos

let get_rev_uint32_64_if_bit s pos attr_type bit =
  if attr_type land bit <> 0 then
    let v, pos = get_rev_uint32_64 s pos in
    Some v, pos
  else None, pos

let get_rev_uint64_if_bit s pos attr_type bit =
  if attr_type land bit <> 0 then
    let v, pos = get_rev_uint64 s pos in
    Some v, pos
  else None, pos


let get_uint32_64_if_bit s pos attr_type bit =
  if attr_type land bit <> 0 then
    let v, pos = LittleEndian.get_uint32_64 s pos in
    Some v, pos
  else None, pos

let get_uint64_if_bit s pos attr_type bit =
  if attr_type land bit <> 0 then
    let v, pos = LittleEndian.get_uint64 s pos in
    Some v, pos
  else None, pos

let read_perf_id_sample ss s =
  if ss.session_attr_sample_id_all then
    let attr_type = ss.session_attr_sample_type in
    let pos = String.length s in
    let id_sample_cpu, pos =
      get_rev_uint64_if_bit s pos attr_type bit_PERF_SAMPLE_CPU in
    let id_sample_stream_id, pos =
      get_rev_uint64_if_bit s pos attr_type bit_PERF_SAMPLE_STREAM_ID in
    let id_sample_id, pos =
      get_rev_uint64_if_bit s pos attr_type bit_PERF_SAMPLE_ID in
    let id_sample_time, pos =
      get_rev_uint64_if_bit s pos attr_type bit_PERF_SAMPLE_TIME in
    let id_sample_pid, pos =
      get_rev_uint32_64_if_bit s pos attr_type bit_PERF_SAMPLE_TID in
    let id_sample_tid, pos =
      get_rev_uint32_64_if_bit s pos attr_type bit_PERF_SAMPLE_TID in
    Some {
      id_sample_cpu;  id_sample_stream_id; id_sample_id;
      id_sample_time; id_sample_tid; id_sample_pid
    }
  else None



let read_format attr_read_format s pos =
  if attr_read_format land bit_PERF_FORMAT_GROUP <> 0 then
    let nrL, pos = LittleEndian.get_uint64 s pos in
    let nr = Int64.to_int nrL in
    let read_time_enabled, pos =
      get_uint64_if_bit s pos attr_read_format bit_PERF_FORMAT_TOTAL_TIME_ENABLED
    in
    let read_time_running, pos =
      get_uint64_if_bit s pos attr_read_format bit_PERF_FORMAT_TOTAL_TIME_RUNNING
    in

    let rec iter i pos items =
      if i = 0 then List.rev items, pos else
        let read_value, pos = LittleEndian.get_uint64 s pos in
        let read_id, pos =
          get_uint64_if_bit s pos attr_read_format bit_PERF_FORMAT_ID
        in
        let item = { read_value; read_id } in
        iter (i-1) pos (item :: items)
    in
    let items, pos = iter nr pos [] in
    let read_cntr = Array.of_list items in
    {
      read_time_enabled; read_time_running;
      read_cntr
    }, pos
  else
    let read_value, pos = LittleEndian.get_uint64 s pos in
    let read_time_enabled, pos =
      get_uint64_if_bit s pos attr_read_format bit_PERF_FORMAT_TOTAL_TIME_ENABLED
    in
    let read_time_running, pos =
      get_uint64_if_bit s pos attr_read_format bit_PERF_FORMAT_TOTAL_TIME_RUNNING
    in
    let read_id, pos =
      get_uint64_if_bit s pos attr_read_format bit_PERF_FORMAT_ID
    in
    {
      read_time_enabled; read_time_running;
      read_cntr = [| { read_value;  read_id } |]
    }, pos


let perf_record_mmap s pos =
  let mmap_pid, pos = LittleEndian.get_uint s pos in
  let mmap_tid, pos = LittleEndian.get_uint s pos in
  let mmap_addr, pos = LittleEndian.get_uint64 s pos in
  let mmap_len, pos = LittleEndian.get_uint64 s pos in
  let mmap_pgoff, pos = LittleEndian.get_uint64 s pos in
  let mmap_filename =
    try
      let end_pos = String.index_from s pos '\000' in
      String.sub s pos (end_pos - pos)
    with Not_found ->
      String.sub s pos (String.length s - pos) in
  PERF_RECORD_MMAP {
    mmap_pid; mmap_tid; mmap_addr;
    mmap_len; mmap_pgoff; mmap_filename
  }

let perf_record_lost s pos =
  let lost_id, pos = LittleEndian.get_uint64 s pos in
  let lost_lost, pos = LittleEndian.get_uint64 s pos in
  PERF_RECORD_LOST {
    lost_id; lost_lost;
  }

let perf_record_comm s pos =
  let comm_pid, pos = LittleEndian.get_uint s pos in
  let comm_tid, pos = LittleEndian.get_uint s pos in
  let comm_comm =
    try
      let end_pos = String.index_from s pos '\000' in
      String.sub s pos (end_pos - pos)
    with Not_found ->
      String.sub s pos (String.length s - pos) in
  PERF_RECORD_COMM {
    comm_pid; comm_tid; comm_comm;
  }

let perf_record_exit s pos =
  let exit_pid, pos = LittleEndian.get_uint s pos in
  let exit_ppid, pos = LittleEndian.get_uint s pos in
  let exit_tid, pos = LittleEndian.get_uint s pos in
  let exit_ptid, pos = LittleEndian.get_uint s pos in
  let exit_time, pos = LittleEndian.get_uint64 s pos in
  PERF_RECORD_EXIT {
    exit_pid; exit_ppid; exit_tid; exit_ptid; exit_time;
  }

let perf_record_fork s pos =
  let fork_pid, pos = LittleEndian.get_uint s pos in
  let fork_ppid, pos = LittleEndian.get_uint s pos in
  let fork_tid, pos = LittleEndian.get_uint s pos in
  let fork_ptid, pos = LittleEndian.get_uint s pos in
  let fork_time, pos = LittleEndian.get_uint64 s pos in
  PERF_RECORD_FORK {
    fork_pid; fork_ppid; fork_tid; fork_ptid; fork_time;
  }

let perf_record_read ss s pos =
  let read_pid, pos = LittleEndian.get_uint s pos in
  let read_tid, pos = LittleEndian.get_uint s pos in
  let read_format, pos = read_format ss.session_attr_read_format s pos in
  PERF_RECORD_READ {
    read_pid; read_tid; read_format
  }

let perf_record_throttle s pos =
  let throttle_time, pos = LittleEndian.get_uint64 s pos in
  let throttle_id, pos = LittleEndian.get_uint64 s pos in
  let throttle_stream_id, pos = LittleEndian.get_uint64 s pos in
  {
    throttle_time; throttle_id; throttle_stream_id
  }

let perf_record_sample ss s pos =
  let sample_type = ss.session_attr_sample_type in
  let sample_ip, pos =
    get_uint64_if_bit s pos sample_type bit_PERF_SAMPLE_IP in
  let sample_pid, pos =
    get_uint32_64_if_bit s pos sample_type bit_PERF_SAMPLE_TID in
  let sample_tid, pos =
    get_uint32_64_if_bit s pos sample_type bit_PERF_SAMPLE_TID in
  let sample_time, pos =
    get_uint64_if_bit s pos sample_type bit_PERF_SAMPLE_TIME in
  let sample_addr, pos =
    get_uint64_if_bit s pos sample_type bit_PERF_SAMPLE_ADDR in
  let sample_id, pos =
    get_uint64_if_bit s pos sample_type bit_PERF_SAMPLE_ID in
  let sample_stream_id, pos =
    get_uint64_if_bit s pos sample_type bit_PERF_SAMPLE_STREAM_ID in
  let sample_cpu, pos =
    get_uint32_64_if_bit s pos sample_type bit_PERF_SAMPLE_CPU in
  let sample_res, pos =
    get_uint32_64_if_bit s pos sample_type bit_PERF_SAMPLE_CPU in
  let sample_period, pos =
    get_uint64_if_bit s pos sample_type bit_PERF_SAMPLE_PERIOD in

  let attr_read_format =
    match sample_id with
        None -> ss.session_attr_read_format
      | Some id ->
        try
          let rec find_attr id list =
            match list with
                [] -> raise Not_found
              | (attr, (begin_id, size_id)) :: list ->
                (* TODO: Check semantics *)
                find_attr id list
          in
          let attr = find_attr id ss.session_attrs in
          attr.attr_read_format
        with Not_found ->
          ss.session_attr_read_format
  in

  let sample_read, pos =
    if  sample_type land bit_PERF_SAMPLE_READ <> 0 then
      let (v, pos) = read_format attr_read_format s pos in
      Some v, pos
    else None, pos in
  let sample_callchain, pos =
    if  sample_type land bit_PERF_SAMPLE_CALLCHAIN <> 0 then
      let nr, pos = LittleEndian.get_uint64 s pos in
      let nr = Int64.to_int nr in
      let ips = Array.init nr (fun i ->
        let v, _ = LittleEndian.get_uint64 s (pos + i * 8) in
        v) in
      let pos = pos + nr * 8 in
      Some ips, pos
    else None, pos in
  let sample_raw, pos =
    if  sample_type land bit_PERF_SAMPLE_RAW <> 0 then
      let size, pos = LittleEndian.get_uint s pos in
      let data = String.sub s pos size in
      Some data, pos + size
    else None, pos in

  PERF_RECORD_SAMPLE {
    sample_ip ; sample_pid ; sample_tid ;
    sample_time ; sample_addr ; sample_id ; sample_stream_id ; sample_cpu ;
    sample_res ; sample_period ; sample_read ; sample_callchain ; sample_raw
  }

(* TODO: try to detect when the format of an event is not what we were expecting *)

let read_perf_event_type ss ev_type ev_misc s =
  if !verbose > 1 then
    Printf.eprintf "ev_type = %d, rem_size = %d\n" ev_type (String.length s);
  let ev_type =
  match ev_type with
    | 1 -> perf_record_mmap s 0
    | 2 -> perf_record_lost s 0
    | 3 -> perf_record_comm s 0
    | 4 -> perf_record_exit s 0
    | 5 -> PERF_RECORD_THROTTLE (perf_record_throttle s 0)
    | 6 -> PERF_RECORD_UNTHROTTLE (perf_record_throttle s 0)
    | 7 -> perf_record_fork s 0
    | 8 -> perf_record_read ss s 0
    | 9 -> perf_record_sample ss s 0
    | _ -> PERF_RECORD_UNKNOWN (ev_type, s)
  in
  {
    ev_type;
    ev_misc;
    ev_id_sample = (match ev_type with
        PERF_RECORD_SAMPLE _ -> None
      | _ -> read_perf_id_sample ss s);
}

let read ?(on_attr = (fun _attr _ids -> ())) ~on_event ~filename =
  let ic = open_in_bin filename in
  let buf = Bytes.create 8 in
  really_input ic buf 0 8;
  let magic = Bytes.to_string buf in
  if magic <> "PERFFILE" && magic <> "PERFILE2" then
    failwith (Printf.sprintf "Error: file %S has bad magic %S\n%!"
                filename (Bytes.to_string buf))
  ;

  let sizeL, pos =
    really_input ic buf 0 8;
    LittleEndian.Byt.get_int64 buf 0 in
  let size = Int64.to_int sizeL in
  if !verbose > 0 then
    Printf.eprintf "Size of PERFFILE header = %d\n%!" size;
  assert (size >= expected_header_size);
  if !verbose >= 0 && size > expected_header_size then begin
    Printf.eprintf "Warning: header size %d > %d, new version ?\n%!"
      size expected_header_size
  end;

  let rem_size = Int64.to_int sizeL - 8 in
  let s = Bytes.create rem_size in
  really_input ic s 0 rem_size;
  let s = Bytes.to_string s in

  let attr_sizeL, pos = LittleEndian.get_int64 s 0 in
  if !verbose > 0 then
    Printf.eprintf "Size of PERFFILE attr = %Ld\n%!" attr_sizeL;

  let attrs, pos = read_perf_file_section s pos in
  let data, pos = read_perf_file_section s pos in
  let event_types, pos = read_perf_file_section s pos in

  let _adds_features = String.sub s pos 32 in

  if !verbose > 0 then begin
    Printf.eprintf "attrs: at %Ld for %Ld bytes\n%!" (fst attrs) (snd attrs);
    Printf.eprintf "data: at %Ld for %Ld bytes\n%!" (fst data) (snd data);
    Printf.eprintf "evtypes: at %Ld for %Ld bytes\n%!" (fst event_types) (snd event_types);
  end;

  let event_types =
    let event_type_pos = Int64.to_int (fst event_types) in
    if event_type_pos = 0 then
      Int64Map.empty
    else
    let event_type_size = Int64.to_int (snd event_types) in
    seek_in ic event_type_pos;
    let event_types = Bytes.create event_type_size in
    really_input ic event_types 0 event_type_size;
    let event_types = Bytes.to_string event_types in
    let nevent_types = event_type_size / event_type_size in

    let rec iter nevent_types =
      if nevent_types = 0 then Int64Map.empty else
        let nevent_types = nevent_types - 1 in
        let pos = nevent_types * event_type_size in
        let event_id, pos = LittleEndian.get_int64 event_types pos in
        let event_name_len = try
                    (String.index_from event_types pos '\000') - pos
          with Not_found -> 64 in
        let event_name = String.sub event_types pos event_name_len in
        if !verbose > 0 then
          Printf.eprintf "event %Ld : %S\n%!" event_id event_name;
        let map = iter nevent_types in
        Int64Map.add event_id event_name map
    in
    iter nevent_types
  in
  (* for example:
     event 0 : "cycles\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
  *)

  let attrs =
    let attr_pos = Int64.to_int (fst attrs) in
    let attr_size = Int64.to_int (snd attrs) in
    seek_in ic attr_pos;
    let attrs =
      let s = Bytes.create attr_size in
      really_input ic s 0 attr_size;
      Bytes.to_string s
    in

    let rec iter pos =
      if pos = attr_size then [] else
        let attr, pos = read_perf_event_attr event_types attrs pos in
        let ids, pos = read_perf_file_section attrs pos in
        on_attr attr ids;
        (attr, ids) :: iter pos
    in
    iter 0

  in

(* TODO : do something else !! Normally, there is a way to know which attr
   to choose to parse events... *)
  let ss =
    match attrs with
        [] -> assert false
      | (attr, ids) :: _ ->
        {
          session_attrs = attrs;
          session_attr_read_format = attr.attr_read_format;
          session_attr_sample_type = attr.attr_sample_type;
          session_attr_sample_id_all = attr.attr_sample_id_all;
        }
  in

  let perf_event_header = Bytes.create perf_event_header_size in
  let rec iter size nevents =
    if size > 0 then begin
      really_input ic perf_event_header 0 perf_event_header_size;
      let perf_event_header = Bytes.to_string perf_event_header in
      let ev_type, pos = LittleEndian.get_uint perf_event_header 0 in
      let ev_misc, pos = LittleEndian.get_uint16 perf_event_header pos in
      let ev_size, pos = LittleEndian.get_uint16 perf_event_header pos in
(*      Printf.printf "ev_size = %d\n" ev_size; *)
      let rem_size = ev_size - perf_event_header_size in
      let ev =
        let ev = Bytes.create rem_size in
        really_input ic ev 0 rem_size;
        Bytes.to_string ev
      in

      let ev = read_perf_event_type ss ev_type ev_misc ev in
      on_event ev;
      iter (size - ev_size) (nevents+1)
    end else nevents
  in
  let pos = Int64.to_int (fst data) in
  let size = Int64.to_int (snd data) in
  seek_in ic pos;
  iter size 0
