type 'a t = 'a option

let (>>=) opt f =
    match opt with
    | Some(x) -> f x
    | None -> None
