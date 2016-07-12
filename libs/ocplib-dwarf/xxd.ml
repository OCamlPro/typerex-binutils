(*From Practical OCaml*)

let make_printable i_char =
  match i_char with
  | n when (((Char.code n) < 32) || ((Char.code n) > 126)) -> '.'
  | _ -> i_char

let make_hex chr = Printf.sprintf "%.2x" (Char.code chr)

let conditional_add_st bffr ch =
  match bffr with
  | n when ((Buffer.length bffr) = 0) -> Buffer.add_string bffr ch
  | n when ((Buffer.length bffr) = 4) -> Buffer.add_string bffr (" " ^ ch)
  | n when (((Buffer.length bffr) mod 5) = 4) -> Buffer.add_string bffr (" " ^ ch)
  | _ -> Buffer.add_string bffr ch

let string_map str fnc =
  let rec strmap st acc =
    match st with
    | "" -> List.rev acc
    | _ -> strmap (String.sub st 1 ((String.length st) - 1)) ((fnc st.[0]) :: acc)
  in strmap str []

let rec output_lines s f_buf s_buf curpos len =
  try
    let res = if len < 16 then len else 16 in
    let str_buf = Bytes.create 16 in
    Bytes.blit_string s curpos str_buf 0 res;
    (
      if (res < 16) then
        (List.iter (conditional_add_st f_buf)
           (string_map (String.sub str_buf 0 res) make_hex);
         List.iter (Buffer.add_char s_buf)
           (string_map (String.sub str_buf 0 res ) make_printable))
      else
        (List.iter (conditional_add_st f_buf) (string_map str_buf make_hex);
         List.iter (Buffer.add_char s_buf) (string_map str_buf make_printable))
    );
    Printf.printf "%0.7x: %-40s %s\n" curpos (Buffer.contents f_buf)
      (Buffer.contents s_buf);

    if (res < 16) then
      raise Exit
    else
      Buffer.clear f_buf;
    Buffer.clear s_buf;
    output_lines s f_buf s_buf (curpos + 16) (len - 16)
  with Exit -> ()
