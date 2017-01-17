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

