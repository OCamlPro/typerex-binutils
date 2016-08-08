
type mode =
  | OK
  | SkipOffset
  | SkipComment
  | SkipLabel

let _ =
  let b = Buffer.create 1111 in
  let ic = open_in Sys.argv.(1) in
  let rec iter mode =
    match mode, input_char ic with
    | SkipOffset, ':' -> iter OK
    | SkipOffset, _ -> iter SkipOffset

    | OK, '#' -> iter SkipComment
    | OK, '<' -> iter SkipLabel
    | OK, '\n' -> Buffer.add_char b '\n'; iter SkipOffset
    | OK, c -> Buffer.add_char b c; iter OK

    | SkipComment, '\n' -> Buffer.add_char b '\n'; iter SkipOffset
    | SkipComment, _ -> iter SkipComment

    | SkipLabel, '>' -> Buffer.add_char b '_'; iter OK
    | SkipLabel, _ -> iter SkipLabel

  in
  (try iter SkipOffset with End_of_file -> ());
  close_in ic;
  let oc = open_out (Sys.argv.(1) ^ ".simple") in
  output_string oc (Buffer.contents b);
  close_out oc

