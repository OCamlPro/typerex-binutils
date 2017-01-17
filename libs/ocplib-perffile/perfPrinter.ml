(**************************************************************************)
(*                                                                        *)
(*                        OCamlPro Typerex                                *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the LGPL v3.0            *)
(*   (GNU Lesser General Public Licence version 3.0).                     *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)


open PerfTypes

let perf_type_id = function
  | PERF_TYPE_HARDWARE -> "PERF_TYPE_HARDWARE"
  | PERF_TYPE_SOFTWARE -> "PERF_TYPE_SOFTWARE"
  | PERF_TYPE_TRACEPOINT -> "PERF_TYPE_TRACEPOINT"
  | PERF_TYPE_HW_CACHE -> "PERF_TYPE_HW_CACHE"
  | PERF_TYPE_RAW -> "PERF_TYPE_RAW"
  | PERF_TYPE_BREAKPOINT -> "PERF_TYPE_BREAKPOINT"
  | PERF_TYPE_UNKNOWN n -> Printf.sprintf "PERF_TYPE_UNKNOWN %d" n

let perf_hw_id = function
  | PERF_COUNT_HW_CPU_CYCLES -> "PERF_COUNT_HW_CPU_CYCLES"
  | PERF_COUNT_HW_INSTRUCTIONS -> "PERF_COUNT_HW_INSTRUCTIONS"
  | PERF_COUNT_HW_CACHE_REFERENCES -> "PERF_COUNT_HW_CACHE_REFERENCES"
  | PERF_COUNT_HW_CACHE_MISSES -> "PERF_COUNT_HW_CACHE_MISSES"
  | PERF_COUNT_HW_BRANCH_INSTRUCTIONS -> "PERF_COUNT_HW_BRANCH_INSTRUCTIONS"
  | PERF_COUNT_HW_BRANCH_MISSES -> "PERF_COUNT_HW_BRANCH_MISSES"
  | PERF_COUNT_HW_BUS_CYCLES -> "PERF_COUNT_HW_BUS_CYCLES"
  | PERF_COUNT_HW_STALLED_CYCLES_FRONTEND ->
    "PERF_COUNT_HW_STALLED_CYCLES_FRONTEND"
  | PERF_COUNT_HW_STALLED_CYCLES_BACKEND ->
    "PERF_COUNT_HW_STALLED_CYCLES_BACKEND"
  | PERF_COUNT_HW_UNKNOWN n -> Printf.sprintf "PERF_COUNT_HW_UNKNOWN %d" n

let perf_hw_cache_id = function
  | PERF_COUNT_HW_CACHE_L1D -> "PERF_COUNT_HW_CACHE_L1D"
  | PERF_COUNT_HW_CACHE_L1I -> "PERF_COUNT_HW_CACHE_L1I"
  | PERF_COUNT_HW_CACHE_LL -> "PERF_COUNT_HW_CACHE_LL"
  | PERF_COUNT_HW_CACHE_DTLB -> "PERF_COUNT_HW_CACHE_DTLB"
  | PERF_COUNT_HW_CACHE_ITLB -> "PERF_COUNT_HW_CACHE_ITLB"
  | PERF_COUNT_HW_CACHE_BPU -> "PERF_COUNT_HW_CACHE_BPU"
  | PERF_COUNT_HW_CACHE_NODE -> "PERF_COUNT_HW_CACHE_NODE"
  | PERF_COUNT_HW_CACHE_UNKNOWN n ->
    Printf.sprintf "PERF_COUNT_HW_CACHE_UNKNOWN %d" n

let perf_hw_cache_op_id = function
  | PERF_COUNT_HW_CACHE_OP_READ -> "PERF_COUNT_HW_CACHE_OP_READ"
  | PERF_COUNT_HW_CACHE_OP_WRITE -> "PERF_COUNT_HW_CACHE_OP_WRITE"
  | PERF_COUNT_HW_CACHE_OP_PREFETCH -> "PERF_COUNT_HW_CACHE_OP_PREFETCH"
  | PERF_COUNT_HW_CACHE_OP_UNKNOWN n ->
    Printf.sprintf "PERF_COUNT_HW_CACHE_OP_UNKNOWN %d" n

let read_format b r =
  Printf.bprintf b "   READ_FORMAT {\n";
  (match r.read_time_enabled with None -> () | Some v ->
    Printf.bprintf b "      time_enabled = %Ld\n" v);
  (match r.read_time_running with None -> () | Some v ->
    Printf.bprintf b "      time_running = %Ld\n" v);
  Array.iter (fun r ->
    Printf.bprintf b "      (";
    (match r.read_id with None -> () | Some v ->
      Printf.bprintf b "id = %Ld; " v);
    Printf.bprintf b "value = %Ld)\n" r.read_value
  ) r.read_cntr;
  Printf.bprintf b "   }\n"

let perf_record_mmap b p =
  Printf.bprintf b "PERF_RECORD_MMAP {\n";
  Printf.bprintf b "   pid = %d\n" p.mmap_pid;
  Printf.bprintf b "   tid = %d\n" p.mmap_tid;
  Printf.bprintf b "   addr = %Lx\n" p.mmap_addr;
  Printf.bprintf b "   len = %Ld\n" p.mmap_len;
  Printf.bprintf b "   pgoff = %Ld\n" p.mmap_pgoff;
  Printf.bprintf b "   filename = %S\n" p.mmap_filename;
  Printf.bprintf b "}\n"

let perf_record_lost b p =
  Printf.bprintf b "PERF_RECORD_LOST {\n";
  Printf.bprintf b "   id = %Ld\n" p.lost_id;
  Printf.bprintf b "   lost = %Ld\n" p.lost_lost;
  Printf.bprintf b "}\n"

let perf_record_comm b p =
  Printf.bprintf b "PERF_RECORD_COMM {\n";
  Printf.bprintf b "   pid = %d\n" p.comm_pid;
  Printf.bprintf b "   tid = %d\n" p.comm_tid;
  Printf.bprintf b "   comm = %S\n" p.comm_comm;
  Printf.bprintf b "}\n"

let perf_record_read b p =
  Printf.bprintf b "PERF_RECORD_READ {\n";
  Printf.bprintf b "   pid = %d\n" p.read_pid;
  Printf.bprintf b "   tid = %d\n" p.read_tid;
  read_format b p.read_format;
  Printf.bprintf b "}\n"

let perf_record_throttle b s p =
  Printf.bprintf b "PERF_RECORD_%s {\n" s;
  Printf.bprintf b "   time = %Ld\n" p.throttle_time;
  Printf.bprintf b "   id = %Ld\n" p.throttle_id;
  Printf.bprintf b "   stream_id = %Ld\n" p.throttle_stream_id;
  Printf.bprintf b "}\n"

let perf_record_exit b p =
  Printf.bprintf b "PERF_RECORD_EXIT {\n";
  Printf.bprintf b "   pid = %d\n" p.exit_pid;
  Printf.bprintf b "   ppid = %d\n" p.exit_ppid;
  Printf.bprintf b "   tid = %d\n" p.exit_tid;
  Printf.bprintf b "   ptid = %d\n" p.exit_ptid;
  Printf.bprintf b "   time = %Ld\n" p.exit_time;
  Printf.bprintf b "}\n"

let perf_record_fork b p =
  Printf.bprintf b "PERF_RECORD_FORK {\n";
  Printf.bprintf b "   pid = %d\n" p.fork_pid;
  Printf.bprintf b "   ppid = %d\n" p.fork_ppid;
  Printf.bprintf b "   tid = %d\n" p.fork_tid;
  Printf.bprintf b "   ptid = %d\n" p.fork_ptid;
  Printf.bprintf b "   time = %Ld\n" p.fork_time;
  Printf.bprintf b "}\n"

let perf_record_sample b sample =
  Printf.bprintf b "PERF_RECORD_SAMPLE {\n";
      (match sample.sample_ip with None -> () | Some v ->
        Printf.bprintf b "   ip = %Lx\n" v);
      (match sample.sample_pid with None -> () | Some v ->
        Printf.bprintf b "   pid = %Ld\n" v);
      (match sample.sample_tid with None -> () | Some v ->
        Printf.bprintf b "   tid = %Ld\n" v);
      (match sample.sample_time with None -> () | Some v ->
        Printf.bprintf b "   time = %Ld\n" v);
      (match sample.sample_addr with None -> () | Some v ->
        Printf.bprintf b "   addr = %Ld\n" v);
      (match sample.sample_id with None -> () | Some v ->
        Printf.bprintf b "   id = %Ld\n" v);
      (match sample.sample_stream_id with None -> () | Some v ->
        Printf.bprintf b "   stream_id = %Ld\n" v);
      (match sample.sample_cpu with None -> () | Some v ->
        Printf.bprintf b "   cpu = %Ld\n" v);
      (match sample.sample_res with None -> () | Some v ->
        Printf.bprintf b "   res = %Ld\n" v);
      (match sample.sample_period with None -> () | Some v ->
        Printf.bprintf b "   period = %Ld\n" v);
      (match sample.sample_read with None -> () | Some v ->
        read_format b v);
      (match sample.sample_callchain with None -> () | Some v ->
        Printf.bprintf b "   callchain = %d\n" (Array.length v));
      (match sample.sample_raw with None -> () | Some v ->
        Printf.bprintf b "   raw = %d\n" (String.length v));
  Printf.bprintf b "}\n"

let perf_event_type b ev =
  match ev with
    | PERF_RECORD_MMAP p -> perf_record_mmap b p
    | PERF_RECORD_LOST p -> perf_record_lost b p
    | PERF_RECORD_COMM p -> perf_record_comm b p
    | PERF_RECORD_EXIT p -> perf_record_exit b p
    | PERF_RECORD_THROTTLE p -> perf_record_throttle b "THROTTLE" p
    | PERF_RECORD_UNTHROTTLE p -> perf_record_throttle b "UNTHROTTLE" p
    | PERF_RECORD_FORK p -> perf_record_fork b p
    | PERF_RECORD_READ p -> perf_record_read b p
    | PERF_RECORD_SAMPLE p -> perf_record_sample b p
    | PERF_RECORD_UNKNOWN (ev_type, s) ->
      Printf.bprintf b "PERF_RECORD_UNKNOWN (%d,%d)\n"
        ev_type (String.length s)

let perf_id_sample b sample =
  match sample with
      None -> ()
    | Some sample ->
      Printf.bprintf b "at sample {\n";
      (match sample.id_sample_cpu with None -> () | Some v ->
        Printf.bprintf b "   cpu = %Ld\n" v);
      (match sample.id_sample_stream_id with None -> () | Some v ->
        Printf.bprintf b "   stream_id = %Ld\n" v);
      (match sample.id_sample_id with None -> () | Some v ->
        Printf.bprintf b "   id = %Ld\n" v);
      (match sample.id_sample_time with None -> () | Some v ->
        Printf.bprintf b "   time = %Ld\n" v);
      (match sample.id_sample_tid with None -> () | Some v ->
        Printf.bprintf b "   tid = %Ld\n" v);
      (match sample.id_sample_pid with None -> () | Some v ->
        Printf.bprintf b "   pid = %Ld\n" v);
      Printf.bprintf b "}\n"

let b = Buffer.create 300
let perf_event_type ev =
  Buffer.clear b;
  perf_event_type b ev.ev_type;
  Printf.bprintf b "with ev_misc = %d\n" ev.ev_misc;
  perf_id_sample b ev.ev_id_sample;
  Buffer.contents b

let attr_sample = function
  | AttrSamplePeriod n -> Printf.sprintf "period %Ld" n
  | AttrSampleFreq n -> Printf.sprintf "freq %Ld" n

let perf_event_attr attr =
  Buffer.clear b;
  Printf.bprintf b "PERF_EVENT_ATTR {\n";
  Printf.bprintf b "   type : %s\n" (perf_type_id attr.attr_type);
  Printf.bprintf b "   size : %d\n" attr.attr_size;
  Printf.bprintf b "   config: %s\n" (match attr.attr_config_name with
    None -> Int64.to_string attr.attr_config
    | Some s -> s);
  Printf.bprintf b "   sample: %s\n"(attr_sample attr.attr_sample);
  Printf.bprintf b "   sample_type: %d\n" attr.attr_sample_type;
  Printf.bprintf b "   read_format: %d\n" attr.attr_read_format;

  Printf.bprintf b "   disabled = %b\n%!" attr.attr_disabled;
  Printf.bprintf b "   inherit = %b\n%!" attr.attr_inherit;
  Printf.bprintf b "   pinned = %b\n%!" attr.attr_pinned;
  Printf.bprintf b "   exclusive = %b\n%!" attr.attr_exclusive;
  Printf.bprintf b "   exclude_user = %b\n%!" attr.attr_exclude_user;
  Printf.bprintf b "   exclude_kernel = %b\n%!" attr.attr_exclude_kernel;
  Printf.bprintf b "   exclude_hv = %b\n%!" attr.attr_exclude_hv;
  Printf.bprintf b "   exclude_idle = %b\n%!" attr.attr_exclude_idle;

  Printf.bprintf b "   mmap = %b\n%!" attr.attr_mmap;
  Printf.bprintf b "   comm = %b\n%!" attr.attr_comm;
  Printf.bprintf b "   freq = %b\n%!" attr.attr_freq;
  Printf.bprintf b "   inherit_stat = %b\n%!" attr.attr_inherit_stat;
  Printf.bprintf b "   enable_on_exec = %b\n%!" attr.attr_enable_on_exec;
  Printf.bprintf b "   task = %b\n%!" attr.attr_task;
  Printf.bprintf b "   watermark = %b\n%!" attr.attr_watermark;
  Printf.bprintf b "   precise_ip = %d\n%!" attr.attr_precise_ip;
  Printf.bprintf b "   mmap_data = %b\n%!" attr.attr_mmap_data;
  Printf.bprintf b "   sample_id_all = %b\n%!" attr.attr_sample_id_all;

  Printf.bprintf b "}\n";
  Buffer.contents b
