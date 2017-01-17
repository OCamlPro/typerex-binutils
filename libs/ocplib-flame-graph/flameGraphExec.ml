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
