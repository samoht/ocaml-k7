(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let rec drain ~read ~feed buf =
  match read buf 0 (Bytes.length buf) with
  | 0 -> ()
  | n ->
      feed buf n;
      drain ~read ~feed buf
  | exception Unix.Unix_error (EINTR, _, _) -> drain ~read ~feed buf
  | exception Unix.Unix_error _ -> ()

let rec run ~read ~ready ~pause ~exited ~feed buf =
  match read buf 0 (Bytes.length buf) with
  | 0 -> ended ~read ~ready ~pause ~exited ~feed buf
  | n ->
      feed buf n;
      run ~read ~ready ~pause ~exited ~feed buf
  | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) ->
      if exited () then drain ~read ~feed buf
      else (
        ready ();
        run ~read ~ready ~pause ~exited ~feed buf)
  | exception Unix.Unix_error _ -> ended ~read ~ready ~pause ~exited ~feed buf

and ended ~read ~ready ~pause ~exited ~feed buf =
  if exited () then drain ~read ~feed buf
  else (
    pause ();
    run ~read ~ready ~pause ~exited ~feed buf)
