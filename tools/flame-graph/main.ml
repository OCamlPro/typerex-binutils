
let () =
  let filename = Sys.argv.(1) in
  let bts = FlameGraph.read_folded_file filename in
  let tree = FlameGraph.tree_of_bts bts in
  FlameGraph.set_title tree
    (Printf.sprintf "Flame Graph of %s (by ocp-flamegraph)" filename);
  let s = FlameGraph.SVG.of_tree (FlameGraph.palette FlameGraph.Hot) tree in
  let oc = open_out (filename ^ ".svg") in
  output_string oc s;
  close_out oc
