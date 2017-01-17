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

open StringCompat

let output = ref None
let folded = ref None
let recursive = ref false
let dwarf = ref false

let config = FlameGraph.new_config ()
let frequency = ref 99
let interpolate = ref None

let svg_of_tree tree =
  let tree = if !recursive then FlameGraph.merge_rec tree else tree in
  begin match !folded with
  | None -> ()
  | Some file ->
    let bts = FlameGraph.bts_of_tree tree in
    FlameGraphFile.write_folded file bts;
    Printf.eprintf "Folded file %S generated\n%!" file
  end;
  let palette =
    let map = ref StringMap.empty in
    function name ->
      try
        StringMap.find name !map
      with
      | Not_found ->
        let rgb = config.FlameGraph.palette name in
        map := StringMap.add name rgb !map;
        rgb
  in
  let config = { config with FlameGraph.palette } in
  let s = FlameGraph.SVG.of_tree ~config tree in
  match !output with
  | None | Some "--" ->
    output_string stdout s;
    close_out stdout;
  | Some svg_file ->
    let oc = open_out svg_file in
    output_string oc s;
    close_out oc;
    Printf.eprintf "SVG file %S generated\n%!" svg_file

let svg_of_filename filename =
  let bts = FlameGraphFile.read_folded filename in
  let tree = FlameGraph.tree_of_bts bts in
  FlameGraph.set_title tree
    (Printf.sprintf "Flame Graph of %s (by ocp-flamegraph)" filename);
  match !output with
  | Some _ -> svg_of_tree tree
  | None ->
    output := Some (filename ^ ".svg");
    svg_of_tree tree;
    output := None

let handle_perf_script () =
    let tmp_file = Filename.temp_file "perf" ".perf" in
    let _retcode = Printf.kprintf Sys.command "perf script > %s" tmp_file in
    let ic = open_in tmp_file in
    let tree = FlameGraphPerf.read_perf_script ?interpolate:!interpolate ic in
    svg_of_tree tree

let handle_perf args =
  match args with
  | [] -> Printf.eprintf "Error: --perf expects a command\n%!"
  | command :: _ ->
    let perf_command = "perf" in
    let unwinding =
      if !dwarf then
        [ "--call-graph"; "dwarf" ]
      else
        []
    in
    let args = perf_command ::
      "record" :: "-F" :: string_of_int !frequency :: "-g" ::
      unwinding @ ["--"] @ args in
    Printf.eprintf "Starting '%s'\n%!"
      (String.concat "' '" args);
    let args = Array.of_list args in
    let pid = FlameGraphExec.exec perf_command args in
    if !output = None then begin
      let command = Filename.basename command in
      output := Some (Printf.sprintf "perf-%s-%d.svg" command pid);
    end;
    handle_perf_script ()

let action = ref None
let perf_args = ref []

let () =

  let arg_list = Arg.align [
    "--perf-script", Arg.Unit (fun () ->
      action := Some handle_perf_script),
    " Call 'perf script' and output a flame graph to SVG";

    "--perf", Arg.Tuple [
      Arg.Unit (fun () ->
        action := Some (function () ->
          handle_perf (List.rev !perf_args)
        ));
      Arg.Rest (fun s -> perf_args := s :: !perf_args);
    ],
    "COMMAND Call COMMAND with 'perf' and output SVG";

    "-o", Arg.String (fun s -> output := Some s),
    "OUTPUT.svg Output SVG to this file (-- is stdout)";
    "--output", Arg.String (fun s -> output := Some s),
      "OUTPUT.svg Output SVG to this file (-- is stdout)";

    "--max-depth", Arg.Int (fun n -> config.FlameGraph.max_depth <- n),
      Printf.sprintf "MAX_DEPTH max depth of flame graph (default %d)"
        config.FlameGraph.max_depth;

    "--js", Arg.String (fun s ->
      let s = FileString.read_file s in
      config.FlameGraph.js <- Some s),
    "FILE.js Javascript to include in generated SVG file";

    "-F", Arg.Int (fun n -> frequency := n),
    Printf.sprintf "Freq Frequency for perf (current %d)" !frequency;

    "--interpolate", Arg.String (fun s -> interpolate := Some s),
    "Function Try to interpolate partial stacks within top function Function";

    "--output-folded", Arg.String (fun s -> folded := Some s),
    "FILE Output also in folded format";

    "--recursive", Arg.Set recursive,
    " Merge recursive function calls";

    "--dwarf", Arg.Set dwarf,
    " Use dwarf unwinding instead of frame pointers";

  ] in
  let arg_usage = String.concat "\n" [
    "ocp-flame-graph [OPTIONS] [FOLDED FILES]: Flame Graph Generator";
  ] in

  Arg.parse arg_list svg_of_filename arg_usage;

  match !action with
  | None -> ()
  | Some action -> action ()
