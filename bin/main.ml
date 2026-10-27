(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A standard descriptor closed at exec is the next one k7 opens, such as
   Eio's wake-up pipe, which FreeBSD lets k7 write its output into until it
   blocks. /dev/null opened the other way holds it, so using it fails as the
   closed one did. *)
let hold fd flag =
  match Unix.fstat fd with
  | _ -> ()
  | exception Unix.Unix_error (EBADF, _, _) ->
      let null = Unix.openfile "/dev/null" [ flag ] 0 in
      if null <> fd then (
        Unix.dup2 ~cloexec:false null fd;
        Unix.close null)

let () =
  hold Unix.stdin O_WRONLY;
  hold Unix.stdout O_RDONLY;
  hold Unix.stderr O_RDONLY;
  exit (Cmdliner.Cmd.eval' K7_cli.Cli.cmd)
