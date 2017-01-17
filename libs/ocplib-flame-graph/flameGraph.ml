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

type tree = {
  mutable node_title : string;
  mutable node_width : float;
  mutable node_children : tree StringMap.t;
}

type bts = (string list * float) list

let new_tree name =
  {
    node_title = name;
    node_width = 0.;
    node_children = StringMap.empty;
  }

let set_title node title = node.node_title <- title

let rec enter_bt node stack count =
  node.node_width <- node.node_width +. count;
  match stack with
  | [] -> ()
  | head :: tail ->
    let node =
      try
        StringMap.find head node.node_children
      with Not_found ->
        let child = new_tree head in
        node.node_children <- StringMap.add head child node.node_children;
        child
    in
    enter_bt node tail count

let tree_of_bts bts =
  let node = new_tree "Flame Graph" in
  List.iter (fun (stack, count) -> enter_bt node stack count) bts;
  node

let rec bts_of_tree node =
  let subcount = ref 0. in
  let children = ref [] in
  StringMap.iter (fun _ node ->
    subcount := !subcount +. node.node_width;
    let bts = bts_of_tree node in
    let bts = List.map (fun (stack, count) ->
      (node.node_title :: stack, count)) bts in
    children := bts @ !children
  ) node.node_children;
  let count = node.node_width -. !subcount in
  if count > 0.01 then
    ([node.node_title],count) :: !children
  else !children

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
  let tree2 = new_tree tree.node_title in
  tree2.node_width <- tree.node_width;
  let filtered = ref 0. in
  StringMap.iter (fun name node ->
    if f node then
      tree2.node_children <-
        StringMap.add name (filter_tree f node) tree2.node_children
    else
      filtered := node.node_width +. !filtered
  ) tree.node_children;
  if !filtered > 0.01 then begin
    let node = new_tree ".." in
    node.node_width <- !filtered;
    if f node then
      tree2.node_children <- StringMap.add ".." node tree2.node_children
  end;
  tree2

let rec merge_rec stack =
  match stack with
  | [] -> []
  | n1 :: stack ->
    let stack = merge_rec stack in
    match stack with
    | [] -> n1 :: []
    | n2 :: tailstack ->
      let name_rec = n1 ^ "(*)" in
      if n1 = n2 || n2 = name_rec then
        name_rec :: tailstack
      else
        n1 :: stack

let merge_rec tree =
  let bts = bts_of_tree tree in
  let bts = List.map (fun (stack, count) ->
    merge_rec stack, count) bts in
  tree_of_bts bts

module type DisplayArg = sig

  type t

  val create : title:string ->
    ?js:string ->
    width:float ->
    height:float ->
    unit ->
    t

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

type config = {
  mutable max_depth : int;
  mutable width : int;
  mutable palette : string -> rgb;
  mutable js : string option;
}

let new_config () = {
    max_depth = 30;
    width = 1200;
    palette = palette Hot;
    js = None;
  }

module type DisplayResult = sig
  val of_tree : ?config:config -> tree -> string
end

module Display(S : DisplayArg) = struct

  let of_tree ?config node =
    let config = match config with
        None -> new_config ()
      | Some config -> config
    in
    let max_depth = config.max_depth in
    let width = config.width in

    let width = float width in
    let width_unit = width /. width_of_tree node in

    let node = filter_tree (fun node ->
      node.node_width *. width_unit > 5.
    ) node in

    let depth = min (height_of_tree node) max_depth  in
    let height = depth * 16 in

  let height = float height in

  let svg = S.create ?js:config.js ~width ~height ~title:node.node_title () in

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
      let (red, green, blue) = config.palette node.node_title in
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
