(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let string =
  match Project_version.env with
  | "dev" -> (
      match Build_info.V1.version () with
      | Some v -> Build_info.V1.Version.to_string v
      | None -> "dev")
  | v -> v
