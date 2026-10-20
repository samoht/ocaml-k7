(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Unicode simple case folding, as Go's [unicode.SimpleFold]. *)

val fold : (int * int) list -> (int * int) list
(** [fold ranges] is [ranges], inclusive ranges of code points, with every code
    point that folds to one of them, as Go's [appendFoldedRange] adds them for a
    pattern that ignores case. The result is not normalised. *)
