(*Zipper definition for generic N-ary trees (each node can have any number of children).*)
(*Can be used for serialization into a linear data structure suitable for storage or tramsmission*)

type 'a tree = Branch of 'a * 'a forest
and 'a forest = 'a tree list

type 'a path =
    Top
    | Node of 'a tree list * ('a path * 'a) * 'a tree list

let leaf a = Branch (a, [])

type 'a zipper = 'a tree * 'a path

let move_left = fun (tree, path) ->
match path with
    | Top -> failwith "left of top"
    | Node (l::left, upv, right) -> (l, Node (left, upv,tree::right))
    | Node _ -> failwith "left of first"

let move_right = fun (tree, path) ->
match path with
    | Top -> failwith "right of top"
    | Node (left, upv, r::right) -> (r, Node (tree::left, upv, right))
    | Node _ -> failwith "right of first"

let move_up = fun (tree, path) ->
match path with
    | Top -> failwith "up of top"
    | Node (left, (up, v), right) ->
            (Branch (v, (List.rev left) @ (tree::right)), up)

let move_down = fun (tree, path) ->
match tree with
    | Branch (v, (t1::trees)) ->
            (t1, Node ([], (path, v), trees))
    | _ -> failwith "down of empty"

let current_tree = function (tree, _) -> tree

let current_value = function (tree, _) ->
    match tree with
        | Branch (v, _) -> Some v

let current_value' = function (tree, _) ->
    match tree with
        | Branch (v, _) -> v

let modify_value t = fun (_, p) -> (t, p)

let change_node f = fun ((v,cs), path) ->
    ((f v, cs), path)

let rec loc2tree = function (t, p) ->
match p with
    Top -> t
    | Node (left,(p',v),right) ->
        loc2tree (Branch (v, (List.rev left) @ (t::right)),p')

let tree2loc t = (t,Top)

let go_root l = tree2loc @@ loc2tree l

let rec map_tree f t =
    match t with
    | Branch (x, ts) -> Branch (f x, List.map (map_tree f) ts)

let rec map_path f p =
    match p with
  | Node (ts1, (p,v), ts2) ->
        Node (List.map (map_tree f) ts1, (map_path f p, f v), List.map (map_tree f) ts2)
  | Top -> Top

(*change the root without moving the pointer*)
let change_root f z =
    let rec chPath pv = match pv with
     | (Top,a) -> (Top, f a)
     | (Node (left,pv,right),v) -> (Node (left, chPath pv,right),v) in
    match z with
  | ((a,ts),Top) -> ((f a,ts),Top)
  | (t, Node (left,pv,right)) -> (t, Node (left,chPath pv,right))

let nth loc n =
    let rec nthrec = function
        1 -> move_down loc
      | n -> if n>0 then move_right (nthrec (n-1)) else failwith "nth expects a positive integer" in
    nthrec n

let rec traverse_collect p = List.rev @@ match p with
  | Top -> []
  | Node (_, (p',v), _) -> v :: traverse_collect p'

let delete (_,p) = match p with
    Top -> failwith "delete of top"
  | Node(left,up,r::right) -> (r,Node(left,up,right))
  | Node(l::left,up,[]) -> (l,Node(left,up,[]))
  | Node([],(up, v),[]) -> (Branch (v, []), up)

(*a successor function*)
let go_ahead ((t,p) as s) =
  let rec upsRight t =
    try move_right t with _ -> upsRight (move_up t) in
  match t, p with
  | Branch(_, _::_), Node (_,_,_::_) -> move_down s
  | Branch(_, []), _              -> upsRight s
  | (_, _)              -> move_down s

let go_back ((t,p) as s) =
    let rec downRight s =
    try let s' = move_down s in begin try let s'' = move_right s in downRight s'' with _ -> s' end with _ -> s in
      try let s' = move_left s in downRight s' with _ -> move_up s

(*n-ary versions*)
let rec go_aheadN i st =
  if i < 1 then st else go_aheadN (i-1) (go_ahead st)

let rec go_backN i st =
  if i < 1 then st else go_backN (i-1) (go_back st)

(*first print the childs, then the root*)
let rec fold_tree f t =
    match t with
    | Branch (x,ts)-> f x (List.map (fold_tree f) ts)

let rec fold_tree2 f g =
    function
        | Branch (x,ts)-> f x; g x (List.map (fold_tree2 f g) ts)

(*let rec fold_tree3 f z =*)
    (*function*)
    (*| Branch (x,[]) -> f x z*)
    (*| Branch (x, ts) -> fold_tree3 f (f x (fold_tree3 f z))*)

let insert_right (t,p) r = match p with
    Top -> failwith "insert of top"
  | Node(left,up,right) -> (t,Node(left,up,r::right));;

let insert_left (t,p) l = match p with
    Top -> failwith "insert of top"
  | Node(left,up,right) -> (t,Node(l::left,up,right));;

let insert_down (t,p) t1 = match t with
    | Branch (v, c) -> (Branch (v, c@[t1]), p)

let rec last_child_of_pos ((t,p) as z) = try last_child_of_pos (move_right z) with _ -> z

