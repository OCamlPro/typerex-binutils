open StringCompat


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

let rec list_common_prefix list1 list2 =
  match list1, list2 with
  | h1::tail1, h2 :: tail2 ->
    if h1 = h2 then h1 :: (list_common_prefix tail1 tail2)
    else []
  | _ -> []

(* Read output of "perf script" *)
let read_perf_script ?interpolate ic =
  let node = FlameGraph.new_tree "Flame Graph of Perf Data" in

  let interpolate_started = ref None in
  let to_interpolate = ref [] in
  let flush_interpolated bottom_stack =
    List.iter (fun stack ->
      FlameGraph.enter_bt node (bottom_stack @ stack) 1.
    )  !to_interpolate;
    to_interpolate := []
  in

  let enter_stack stack =
    match interpolate with
    | None -> FlameGraph.enter_bt node stack 1.
    | Some bottom_name ->
      match stack with
        [] -> ()
      | fun_name :: _ ->
        if fun_name = bottom_name then begin
          if !to_interpolate != [] then begin
            match !interpolate_started with
            | None -> assert false
            | Some prev_stack ->
              flush_interpolated (list_common_prefix prev_stack stack);
          end;
          interpolate_started := Some stack;
          FlameGraph.enter_bt node stack 1.
        end else
          match !interpolate_started with
          | None -> FlameGraph.enter_bt node stack 1.
          | Some _ ->
            to_interpolate := stack :: !to_interpolate
  in
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
          enter_stack stack
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
    flush_interpolated [];
    close_in ic;
    node
