open StringCompat

type tree = {
  mutable node_title : string;
  mutable node_width : float;
  mutable node_children : tree StringMap.t;
}

type bts = (string list * float) list

let new_node name =
  {
    node_title = name;
    node_width = 0.;
    node_children = StringMap.empty;
  }

let set_title node title = node.node_title <- title

let tree_of_bts bts =
  let node = new_node "Flame Graph" in
  let rec enter node stack count =
    node.node_width <- node.node_width +. count;
    match stack with
    | [] -> ()
    | head :: tail ->
      let node =
        try
          StringMap.find head node.node_children
        with Not_found ->
          let child = new_node head in
          node.node_children <- StringMap.add head child node.node_children;
          child
      in
      enter node tail count
  in
  List.iter (fun (stack, count) -> enter node stack count) bts;
  node

let read_folded_file filename =
  let bts = ref [] in
  FileString.iter_lines (fun line ->
    if line <> "" then
      let stack,count = OcpString.cut_at line ' ' in
      let stack = OcpString.split stack ';' in
      let count = float_of_string count in
      bts := (stack, count) :: !bts
  ) filename;
  !bts

let rec height_of_tree node =
  let depth = ref 1 in
  StringMap.iter (fun _ node ->
    depth := max !depth (1 + height_of_tree node)
  ) node.node_children;
  !depth

let width_of_tree node = node.node_width


type palette =
| Hot
| Mem
| IO
type rgb = int * int * int

let palette kind name =
  match kind with
  | Hot ->
    let red = 205 + Random.int 50 in
    let green = Random.int 230 in
    let blue = Random.int 55 in
    (red, green, blue)
  | Mem ->
    let red = 205 + Random.int 50 in
    let green = Random.int 230 in
    let blue = Random.int 55 in
    (red, green, blue)
  | IO ->
    let red = 205 + Random.int 50 in
    let green = Random.int 230 in
    let blue = Random.int 55 in
    (red, green, blue)

let rec filter_tree f tree =
  let tree2 = new_node tree.node_title in
  tree2.node_width <- tree.node_width;
  StringMap.iter (fun name node ->
    if f node then
      tree2.node_children <-
        StringMap.add name (filter_tree f node) tree2.node_children
  ) tree.node_children;
  tree2


module type DisplayArg = sig

  type t

  val create : title:string -> width:float -> height:float -> t

  val rectangle :
    t ->
    title:string ->
    caption:string ->
    x:float ->
    y:float ->
    width:float ->
    red:int ->
    green:int ->
    blue:int ->
    unit

  val to_string : t -> string

  val box_height : float

end

module type DisplayResult = sig
  val of_tree : (string -> rgb) -> tree -> string
end

module Display(S : DisplayArg) = struct

  let of_tree palette node =

  let max_depth = 30 in
  let width = 1200 in

  let width = float width in
  let width_unit = width /. width_of_tree node in

  let node = filter_tree (fun node ->
    node.node_width *. width_unit > 5.
  ) node in

  let depth = min (height_of_tree node) max_depth  in
  let height = depth * 16 in

  let height = float height in

  let svg = S.create ~width ~height
    ~title:node.node_title in

  let rec iter level node x y =
    let width = node.node_width *. width_unit in
    if width > 5. && level <= depth then begin
      let title = node.node_title in
      let caption = Printf.sprintf "%s (%.0f samples)"
        node.node_title node.node_width in
      let max_length = int_of_float ((width -. 6.) /. 7.) in
      let title =
        let len = String.length title in
        if len <= max_length then title else
          String.sub title 0 max_length
      in
      let (red, green, blue) = palette node.node_title in
      S.rectangle svg ~title
        ~caption ~x ~y ~width ~red ~green ~blue;
      let x = ref x in
      let y = y +. S.box_height in
      StringMap.iter (fun _ node ->
        x := !x +. iter (level+1) node !x y
      ) node.node_children;
    end;
    width
  in
  let (_width : float) = iter 1 node 0. 0. in

  S.to_string svg

end

module SVG = Display(FlameGraphSVG)
