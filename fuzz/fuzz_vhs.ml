(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

let is_control c = Char.code c < 0x20 || c = '\x7f'

(* Every builder and setting is total: [tape] returns, it never raises, and
   an accepted tape prints lines without control characters. *)
let test_total s t n =
  let c = Char.chr (abs n mod 256) in
  let actions =
    Vhs.
      [
        type' s;
        key (Ctrl c);
        key (Alt c);
        sleep (Ms n);
        wait ~timeout:(Ms n) (Screen s);
        wait (Screen t);
        hidden [ raw s; type' t ];
        raw t;
      ]
  in
  let settings =
    Vhs.
      [
        Font_size n;
        Font_family s;
        Width n;
        Height n;
        Theme t;
        Padding n;
        Typing_speed (Ms n);
        Framerate n;
      ]
  in
  List.iter
    (fun (settings, outputs, actions) ->
      match Vhs.tape ~settings ~outputs actions with
      | Error _ -> ()
      | Ok tape ->
          let printed = Fmt.str "%a" Vhs.pp tape in
          if String.exists (fun c -> is_control c && c <> '\n') printed then
            failf "control character in %S" printed)
    [
      ([], [ s ], actions);
      (settings, [ "demo.gif" ], []);
      ([], [ "demo.gif" ], actions);
      ([], [ t ^ ".gif" ], List.map (fun a -> Vhs.hidden [ a ]) actions);
    ]

let suite =
  ("vhs", [ test_case "tape is total" [ bytes; bytes; int ] test_total ])
