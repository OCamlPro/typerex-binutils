

let exec command args =
  Unix.handle_unix_error (fun () ->
    match Unix.fork () with
    | 0 ->
      Unix.execvp command args
    | pid ->
      Sys.set_signal Sys.sigint
        (Sys.Signal_handle (fun x -> Unix.kill pid x));
      Printf.eprintf "Waiting for pid %d to terminate\n%!" pid;
      let rec wait () =
        try
          let res = Unix.waitpid [] pid in
          begin
            match res with
            | _, Unix.WEXITED 0 ->
              Printf.eprintf "Command terminated successfully.\n%!";
            | _, Unix.WSIGNALED s ->
              Printf.eprintf
                "Command terminated with signal %d.\n%!" s;
            | _, Unix.WEXITED i ->
              Printf.eprintf
                "Error: command exited with error status %d\n%!" i;
            | _, Unix.WSTOPPED _ ->
              assert false (* should not happen *)
          end;
          Sys.set_signal Sys.sigint Sys.Signal_default;
          pid
        with Unix.Unix_error (Unix.EINTR, "waitpid", _) -> wait ()
      in
      wait ())
    ()
