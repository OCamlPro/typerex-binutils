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


(* From linux-3.2.1 *)

type perf_type_id =
| PERF_TYPE_HARDWARE
| PERF_TYPE_SOFTWARE
| PERF_TYPE_TRACEPOINT
| PERF_TYPE_HW_CACHE
| PERF_TYPE_RAW
| PERF_TYPE_BREAKPOINT
| PERF_TYPE_UNKNOWN of int

let perf_type_id n =
  match n with
  | 0 -> PERF_TYPE_HARDWARE
  | 1 -> PERF_TYPE_SOFTWARE
  | 2 -> PERF_TYPE_TRACEPOINT
  | 3 -> PERF_TYPE_HW_CACHE
  | 4 -> PERF_TYPE_RAW
  | 5 -> PERF_TYPE_BREAKPOINT
  | _ -> PERF_TYPE_UNKNOWN n

(*
 * Generalized performance event event_id types, used by the
 * attr.event_id parameter of the sys_perf_event_open()
 * syscall:
 *)
type perf_hw_id =
  (*
   * Common hardware events, generalized by the kernel:
   *)
| PERF_COUNT_HW_CPU_CYCLES
| PERF_COUNT_HW_INSTRUCTIONS
| PERF_COUNT_HW_CACHE_REFERENCES
| PERF_COUNT_HW_CACHE_MISSES
| PERF_COUNT_HW_BRANCH_INSTRUCTIONS
| PERF_COUNT_HW_BRANCH_MISSES
| PERF_COUNT_HW_BUS_CYCLES
| PERF_COUNT_HW_STALLED_CYCLES_FRONTEND
| PERF_COUNT_HW_STALLED_CYCLES_BACKEND
| PERF_COUNT_HW_UNKNOWN of int

let perf_hw_id = function
  | 0 -> PERF_COUNT_HW_CPU_CYCLES
  | 1 -> PERF_COUNT_HW_INSTRUCTIONS
  | 2 -> PERF_COUNT_HW_CACHE_REFERENCES
  | 3 -> PERF_COUNT_HW_CACHE_MISSES
  | 4 -> PERF_COUNT_HW_BRANCH_INSTRUCTIONS
  | 5 -> PERF_COUNT_HW_BRANCH_MISSES
  | 6 -> PERF_COUNT_HW_BUS_CYCLES
  | 7 -> PERF_COUNT_HW_STALLED_CYCLES_FRONTEND
  | 8 -> PERF_COUNT_HW_STALLED_CYCLES_BACKEND
  | n -> PERF_COUNT_HW_UNKNOWN n

(*
 * Generalized hardware cache events:
 *
 *       { L1-D, L1-I, LLC, ITLB, DTLB, BPU, NODE } x
 *       { read, write, prefetch } x
 *       { accesses, misses }
 *)
type perf_hw_cache_id =
| PERF_COUNT_HW_CACHE_L1D
| PERF_COUNT_HW_CACHE_L1I
| PERF_COUNT_HW_CACHE_LL
| PERF_COUNT_HW_CACHE_DTLB
| PERF_COUNT_HW_CACHE_ITLB
| PERF_COUNT_HW_CACHE_BPU
| PERF_COUNT_HW_CACHE_NODE
| PERF_COUNT_HW_CACHE_UNKNOWN of int

let perf_hw_cache_id = function
  | 0 -> PERF_COUNT_HW_CACHE_L1D
  | 1 -> PERF_COUNT_HW_CACHE_L1I
  | 2 -> PERF_COUNT_HW_CACHE_LL
  | 3 -> PERF_COUNT_HW_CACHE_DTLB
  | 4 -> PERF_COUNT_HW_CACHE_ITLB
  | 5 -> PERF_COUNT_HW_CACHE_BPU
  | 6 -> PERF_COUNT_HW_CACHE_NODE
  | n -> PERF_COUNT_HW_CACHE_UNKNOWN n

type perf_hw_cache_op_id =
| PERF_COUNT_HW_CACHE_OP_READ
| PERF_COUNT_HW_CACHE_OP_WRITE
| PERF_COUNT_HW_CACHE_OP_PREFETCH
| PERF_COUNT_HW_CACHE_OP_UNKNOWN of int

let perf_hw_cache_op_id = function
  | 0 -> PERF_COUNT_HW_CACHE_OP_READ
  | 1 -> PERF_COUNT_HW_CACHE_OP_WRITE
  | 2 -> PERF_COUNT_HW_CACHE_OP_PREFETCH
  | n -> PERF_COUNT_HW_CACHE_OP_UNKNOWN n

type perf_hw_cache_op_result_id =
| PERF_COUNT_HW_CACHE_RESULT_ACCESS
| PERF_COUNT_HW_CACHE_RESULT_MISS
| PERF_COUNT_HW_CACHE_RESULT_UNKNOWN of int

let perf_hw_cache_op_result_id = function
  | 0 -> PERF_COUNT_HW_CACHE_RESULT_ACCESS
  | 1 -> PERF_COUNT_HW_CACHE_RESULT_MISS
  | n -> PERF_COUNT_HW_CACHE_RESULT_UNKNOWN n

(*
 * Special "software" events provided by the kernel, even if the hardware
 * does not support performance events. These events measure various
 * physical and sw events of the kernel (and allow the profiling of them as
 * well):
 *)
type perf_sw_ids =
| PERF_COUNT_SW_CPU_CLOCK
| PERF_COUNT_SW_TASK_CLOCK
| PERF_COUNT_SW_PAGE_FAULTS
| PERF_COUNT_SW_CONTEXT_SWITCHES
| PERF_COUNT_SW_CPU_MIGRATIONS
| PERF_COUNT_SW_PAGE_FAULTS_MIN
| PERF_COUNT_SW_PAGE_FAULTS_MAJ
| PERF_COUNT_SW_ALIGNMENT_FAULTS
| PERF_COUNT_SW_EMULATION_FAULTS
| PERF_COUNT_SW_UNKNOWN of int

let perf_sw_ids = function
  | 0 -> PERF_COUNT_SW_CPU_CLOCK
  | 1 -> PERF_COUNT_SW_TASK_CLOCK
  | 2 -> PERF_COUNT_SW_PAGE_FAULTS
  | 3 -> PERF_COUNT_SW_CONTEXT_SWITCHES
  | 4 -> PERF_COUNT_SW_CPU_MIGRATIONS
  | 5 -> PERF_COUNT_SW_PAGE_FAULTS_MIN
  | 6 -> PERF_COUNT_SW_PAGE_FAULTS_MAJ
  | 7 -> PERF_COUNT_SW_ALIGNMENT_FAULTS
  | 8 -> PERF_COUNT_SW_EMULATION_FAULTS
  | n -> PERF_COUNT_SW_UNKNOWN n

(*
 * Bits that can be set in attr.sample_type to request information
 * in the overflow packets.
 *)
let bit_PERF_SAMPLE_IP                          = 1 lsl 0
let bit_PERF_SAMPLE_TID            = 1 lsl 1
let bit_PERF_SAMPLE_TIME         = 1 lsl 2
let bit_PERF_SAMPLE_ADDR         = 1 lsl 3
let bit_PERF_SAMPLE_READ         = 1 lsl 4
let bit_PERF_SAMPLE_CALLCHAIN         = 1 lsl 5
let bit_PERF_SAMPLE_ID            = 1 lsl 6
let bit_PERF_SAMPLE_CPU            = 1 lsl 7
let bit_PERF_SAMPLE_PERIOD         = 1 lsl 8
let bit_PERF_SAMPLE_STREAM_ID         = 1 lsl 9
let bit_PERF_SAMPLE_RAW            = 1 lsl 10

(* currently, addr_sample_type is an int, we might need later to
   increase it to int64 *)


type perf_id_sample = {
  id_sample_cpu : int64 option;
  id_sample_stream_id : int64 option;
  id_sample_id : int64 option;
  id_sample_time : int64 option;
  id_sample_tid : int64 option;
  id_sample_pid : int64 option;
}

type read_format_item = {
  read_value : int64;
  read_id : int64 option;
}

type read_format = {
  read_time_enabled : int64 option;
  read_time_running : int64 option;
  read_cntr : read_format_item array;
}

let bit_PERF_FORMAT_TOTAL_TIME_ENABLED      = 1 lsl 0
let bit_PERF_FORMAT_TOTAL_TIME_RUNNING      = 1 lsl 1
let bit_PERF_FORMAT_ID            = 1 lsl 2
let bit_PERF_FORMAT_GROUP         = 1 lsl 3

(*
 * Hardware event_id to monitor via a performance monitoring event:
 *)

type attr_sample =
| AttrSamplePeriod of int64
| AttrSampleFreq of int64

type attr_wakeup =
| AttrWakeUpEvents of int64
| AttrWakeUpWatermark of int64

(* precise_ip:

   *  0 - SAMPLE_IP can have arbitrary skid
   *  1 - SAMPLE_IP must have constant skid
   *  2 - SAMPLE_IP requested to have 0 skid
   *  3 - SAMPLE_IP must have 0 skid
   *
   *  See also PERF_RECORD_MISC_EXACT_IP
*)

type perf_event_attr = {
   (*     * Major type: hardware/software/tracepoint/etc.    *)
                        attr_type : perf_type_id;    (* __u32 *)
  (*    Size of the attr structure, for fwd/bwd compat. *)
  attr_size : int;   (*  __u32  *)
  (* Type specific configuration information. *)
  attr_config : int64;    (*  __u64  *) (* name in event_types *)
  attr_config_name : string option;
  attr_sample : attr_sample;   (*  __u64  *)
  attr_sample_type : int;   (*  __u64  *)
  attr_read_format : int;   (*  __u64  *)

  attr_disabled       :  bool; (* off by default        *)
  attr_inherit          :  bool; (* children inherit it   *)
  attr_pinned          :  bool; (* must always be on PMU *)
  attr_exclusive      :  bool; (* only group on PMU     *)
  attr_exclude_user   :  bool; (* don't count user      *)
  attr_exclude_kernel :  bool; (* ditto kernel          *)
  attr_exclude_hv     :  bool; (* ditto hypervisor      *)
  attr_exclude_idle   :  bool; (* don't count when idle *)
  attr_mmap           :  bool; (* include mmap data     *)
  attr_comm          :  bool; (* include comm data     *)
  attr_freq           :  bool; (* use freq, not period  *)
  attr_inherit_stat   :  bool; (* per task counts       *)
  attr_enable_on_exec :  bool; (* next exec enables     *)
  attr_task           :  bool; (* trace fork/exit       *)
  attr_watermark      :  bool; (* wakeup_watermark      *)
  attr_precise_ip     :  int; (* 2 bits *) (* skid constraint       *)
  attr_mmap_data      :  bool; (* non-exec mmap data    *)
  attr_sample_id_all  :  bool; (* sample_type all events *)

  attr_exclude_host   :  bool; (* don't count in host   *)
  attr_exclude_guest  :  bool; (* don't count in guest  *)

  attr_attributes   : int64; (* 43 bits of 64 bits *)

  attr_wakeup : attr_wakeup; (*  __u32  *)
  attr_bp_type : int64;   (*  __u32  *)
  attr_bp_addr : int64;   (*  __u32  *)
  attr_bp_len : int64;  (*  __u64  *)
}

type int32_64 = int64

module Int64Map = Map.Make(Int64)

type perf_session = {
  session_attr_sample_id_all : bool;
  session_attr_sample_type : int;
  session_attr_read_format : int;
  session_attrs : (perf_event_attr * (int64 * int64)) list;
}

type perf_record_sample = {
  sample_ip : int64 option;
  sample_pid : int32_64 option;
  sample_tid : int32_64 option;
  sample_time : int64 option;
  sample_addr : int64 option;
  sample_id : int64 option;
  sample_stream_id : int64 option;
  sample_cpu : int32_64 option;
  sample_res : int32_64 option;
  sample_period : int64 option;
  sample_read : read_format option;
  sample_callchain : int64 array option;
  sample_raw : string option;
}


type perf_record_mmap = {
  mmap_pid : int;
  mmap_tid : int;
  mmap_addr : int64;
  mmap_len : int64;
  mmap_pgoff : int64;
  mmap_filename : string;
}

type perf_record_comm = {
  comm_pid : int;
  comm_tid : int;
  comm_comm : string;
}

type perf_record_exit = {
  exit_pid : int;
  exit_ppid : int;
  exit_tid : int;
  exit_ptid : int;
  exit_time : int64;
}

type perf_record_fork = {
  fork_pid : int;
  fork_ppid : int;
  fork_tid : int;
  fork_ptid : int;
  fork_time : int64;
}

type perf_record_read = {
  read_pid : int;
  read_tid : int;
  read_format : read_format;
}

type perf_record_lost = {
  lost_id : int64;
  lost_lost : int64;
}

type perf_record_throttle = {
  throttle_time : int64;
  throttle_id : int64;
  throttle_stream_id : int64;
}

type perf_event_type = {
  ev_type : ev_type;
  ev_misc : int;
  ev_id_sample : perf_id_sample option;
}

and ev_type =
| PERF_RECORD_UNKNOWN of int * string
| PERF_RECORD_MMAP of perf_record_mmap (* Only for PROT_EXEC mappings *)
| PERF_RECORD_LOST of perf_record_lost
| PERF_RECORD_COMM of perf_record_comm
| PERF_RECORD_EXIT of perf_record_exit
| PERF_RECORD_THROTTLE of perf_record_throttle
| PERF_RECORD_UNTHROTTLE of perf_record_throttle
| PERF_RECORD_FORK of perf_record_fork
| PERF_RECORD_READ of perf_record_read
| PERF_RECORD_SAMPLE of perf_record_sample
