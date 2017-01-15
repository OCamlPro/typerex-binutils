open StringCompat
open FlameGraph


let get_function_name line =
  let len = String.length line in
  let rec iter0 i len line =
    match line.[i] with
      ' ' | '\t' ->
      iter0 (i+1) len line
    | _ ->
      iter1 (i+1) len line

  and iter1 i len line =
    if line.[i] = ' ' then
      iter2 (i+1) (i+1) len line
    else
      iter1 (i+1) len line

  and iter2 pos i len line =
    if line.[i] = ' ' then
      String.sub line pos (i-pos)
    else
      iter2 pos (i+1) len line

  in
  let fun_name = iter0 0 len line in
  (*  Printf.eprintf "%S for %S\n%!" fun_name line; *)
  fun_name

(* Read output of "perf script" *)
let read_perf_script ic =
  let log = ref StringMap.empty in
  let node = new_tree "Flame Graph of Perf Data" in
  let lines = ref [] in
  let commit_lines () =
    let record = List.rev !lines in
    lines := [];
    match record with
      [] -> ()
    | header :: stack ->
      assert (header.[0] <> ' ');
      if header.[0] <> ':' then (* in process init *)
        let proc_name, header = OcpString.cut_at header ' ' in
        let pid, header = OcpString.cut_at header ' ' in
        let time, header = OcpString.cut_at header ':' in
        if header <> " cycles: " then
          Printf.eprintf "header = %S\n%!" header
        else
          let stack = List.map get_function_name (List.rev stack) in
          let node = match stack with
            | [] -> node
            | fun_name :: _ ->
              try StringMap.find fun_name !log with Not_found -> node
          in
          enter_bt_log log node stack 1.
  in

  let rec iter () =
    let line = input_line ic in
    let len = String.length line in
    if len = 0 then
      commit_lines ()
    else
      lines := line :: !lines;
    iter ()
  in
  try
    iter ()
  with End_of_file ->
    commit_lines ();
    close_in ic;
    node
