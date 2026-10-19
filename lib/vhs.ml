(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The tape syntax, the key names and the characters [Ctrl] and [Alt] accept
   follow VHS's lexer/lexer.go and parser/parser.go (MIT, Charm) at v0.12.1. *)

type duration = Ms of int

type key =
  | Enter
  | Tab
  | Space
  | Backspace
  | Delete
  | Insert
  | Escape
  | Up
  | Down
  | Left
  | Right
  | Page_up
  | Page_down
  | Scroll_up
  | Scroll_down
  | Ctrl of char
  | Alt of char

type wait = Prompt | Screen of string

type action =
  | Type of string
  | Key of key
  | Sleep of duration
  | Wait of duration option * wait
  | Hidden of action list
  | Raw of string

let type' s = Type s
let key k = Key k
let sleep d = Sleep d
let wait ?timeout w = Wait (timeout, w)
let hidden actions = Hidden actions
let raw line = Raw line

type shell =
  | Bash
  | Zsh
  | Fish
  | Powershell
  | Pwsh
  | Cmd
  | Nushell
  | Osh
  | Xonsh

type setting =
  | Shell of shell
  | Font_size of int
  | Font_family of string
  | Width of int
  | Height of int
  | Theme of string
  | Padding of int
  | Typing_speed of duration
  | Framerate of int

type t = {
  outputs : string list;
  settings : setting list;
  actions : action list;
}

(* Checking *)

let ( let* ) = Result.bind
let err fmt = Fmt.kstr Result.error fmt

let rec iter_result f = function
  | [] -> Ok ()
  | x :: rest ->
      let* () = f x in
      iter_result f rest

let is_control c = Char.code c < 0x20 || c = '\x7f'
let quote_bit = function '"' -> 1 | '\'' -> 2 | '`' -> 4 | _ -> 0

(* A VHS string ends at its quote, a newline or a NUL and has no escapes. *)
let check_quotable what s =
  if String.exists is_control s then
    err "%s %S: holds a control character" what s
  else if String.fold_left (fun m c -> m lor quote_bit c) 0 s = 7 then
    err "%s %S: holds all three quote characters" what s
  else Ok ()

(* The largest Go [time.Duration], in whole milliseconds. *)
let max_ms = 9_223_372_036_854

let check_ms what n =
  if n < 0 then err "%s%dms: negative duration" what n
  else if n > max_ms then
    err "%s%dms: beyond Go's duration range (%dms)" what n max_ms
  else Ok ()

let check_type s =
  if not (String.is_valid_utf_8 s) then err "Type %S: invalid UTF-8" s
  else if String.exists (fun c -> is_control c && c <> '\t' && c <> '\n') s then
    err "Type %S: holds a control character other than tab and newline" s
  else Ok ()

let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let check_chord name valid c =
  if is_letter c || String.contains valid c then Ok ()
  else err "%s+%s: not a VHS key" name (Char.escaped c)

let check_key = function
  | Ctrl c -> check_chord "Ctrl" {|@[\]^-.|} c
  | Alt c -> check_chord "Alt" "[]." c
  | _ -> Ok ()

let check_wait timeout w =
  let* () =
    match timeout with
    | None -> Ok ()
    | Some (Ms 0) -> err "Wait@0ms: the timeout must be positive"
    | Some (Ms n) -> check_ms "Wait@" n
  in
  match w with
  | Prompt -> Ok ()
  | Screen re when String.exists is_control re ->
      err "Wait+Screen %S: holds a control character" re
  | Screen re ->
      Regexp_syntax.check re
      |> Result.map_error (Fmt.str "Wait+Screen %S: %s" re)

let first_word line =
  String.split_on_char ' ' (String.trim line) |> List.filter (( <> ) "")

(* VHS applies [Set] and [Require] only before the first other command. *)
let check_raw ~started line =
  if String.exists is_control line then
    err "raw line %S: holds a control character" line
  else
    match first_word line with
    | ("Hide" | "Show") :: _ -> err "raw line %S: use hidden instead" line
    | ("Set" :: "TypingSpeed" :: _ | "Output" :: _ | []) as _neutral ->
        Ok started
    | w :: _ when w.[0] = '#' -> Ok started
    | ("Set" | "Require") :: _ when started ->
        err "raw line %S: VHS ignores it after the first action" line
    | ("Set" | "Require") :: _ -> Ok false
    | _ -> Ok true

(* [check_actions ~started actions] is whether an action that ends the
   settings has run once [actions] have. *)
let rec check_actions ~started = function
  | [] -> Ok started
  | a :: rest ->
      let* started =
        match a with
        | Type s ->
            let* () = check_type s in
            Ok true
        | Key k ->
            let* () = check_key k in
            Ok true
        | Sleep (Ms n) ->
            let* () = check_ms "Sleep " n in
            Ok true
        | Wait (timeout, w) ->
            let* () = check_wait timeout w in
            Ok true
        | Hidden actions -> check_actions ~started:true actions
        | Raw line -> check_raw ~started line
      in
      check_actions ~started rest

(* Go's [filepath.Ext] is the suffix from the last dot of the last segment. *)
let has_extension path =
  match (String.rindex_opt path '.', String.rindex_opt path '/') with
  | Some dot, Some slash -> dot > slash
  | Some _, None -> true
  | None, _ -> false

let check_output path =
  let* () = check_quotable "Output" path in
  if has_extension path || String.ends_with ~suffix:"/" path then Ok ()
  else err "Output %S: neither an extension nor a trailing slash" path

let check_setting = function
  | Shell _ -> Ok ()
  | Font_size n when n <= 0 -> err "Set FontSize %d: not positive" n
  | Framerate n when n <= 0 -> err "Set Framerate %d: not positive" n
  | Padding n when n < 0 -> err "Set Padding %d: negative" n
  | Font_family f -> check_quotable "Set FontFamily" f
  | Theme t -> check_quotable "Set Theme" t
  | Typing_speed (Ms n) -> check_ms "Set TypingSpeed " n
  | Font_size _ | Framerate _ | Padding _ | Width _ | Height _ -> Ok ()

(* VHS refuses a frame smaller than twice the padding, its defaults
   included. *)
let check_size settings =
  let last f default =
    List.fold_left (fun v s -> Option.value (f s) ~default:v) default settings
  in
  let padding = last (function Padding n -> Some n | _ -> None) 60 in
  let width = last (function Width n -> Some n | _ -> None) 1200 in
  let height = last (function Height n -> Some n | _ -> None) 600 in
  if width < 2 * padding then
    err "Set Width %d: below twice the padding (%d)" width padding
  else if height < 2 * padding then
    err "Set Height %d: below twice the padding (%d)" height padding
  else Ok ()

let tape ?(settings = []) ~outputs actions =
  let* () =
    if outputs = [] then err "no output" else iter_result check_output outputs
  in
  let* () = iter_result check_setting settings in
  let* () = check_size settings in
  let* _started = check_actions ~started:false actions in
  Ok { outputs; settings; actions }

(* Printing *)

let shell_name = function
  | Bash -> "bash"
  | Zsh -> "zsh"
  | Fish -> "fish"
  | Powershell -> "powershell"
  | Pwsh -> "pwsh"
  | Cmd -> "cmd"
  | Nushell -> "nu"
  | Osh -> "osh"
  | Xonsh -> "xonsh"

let key_name = function
  | Enter -> "Enter"
  | Tab -> "Tab"
  | Space -> "Space"
  | Backspace -> "Backspace"
  | Delete -> "Delete"
  | Insert -> "Insert"
  | Escape -> "Escape"
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"
  | Page_up -> "PageUp"
  | Page_down -> "PageDown"
  | Scroll_up -> "ScrollUp"
  | Scroll_down -> "ScrollDown"
  | Ctrl c -> "Ctrl+" ^ String.make 1 c
  | Alt c -> "Alt+" ^ String.make 1 c

(* [add_quoted b s ~first ~last ~quotes] adds [s.[first..last-1]] quoted with
   a character absent from [quotes], the bits of the quotes it holds. *)
let add_quoted b s ~first ~last ~quotes =
  let q =
    if quotes land 1 = 0 then '"' else if quotes land 2 = 0 then '\'' else '`'
  in
  Buffer.add_char b q;
  Buffer.add_substring b s first (last - first);
  Buffer.add_char b q

let add_string b s =
  let quotes = String.fold_left (fun m c -> m lor quote_bit c) 0 s in
  add_quoted b s ~first:0 ~last:(String.length s) ~quotes

(* A [Type] line per run of [s.[first..last-1]] without all three quotes. *)
let add_text b s ~first ~last =
  let line start stop quotes =
    Buffer.add_string b "Type ";
    add_quoted b s ~first:start ~last:stop ~quotes;
    Buffer.add_char b '\n'
  in
  let rec go start i quotes =
    if i = last then line start i quotes
    else
      let bit = quote_bit s.[i] in
      if bit = 0 || quotes land bit <> 0 then go start (i + 1) quotes
      else if quotes lor bit = 7 then (
        line start i quotes;
        go i (i + 1) bit)
      else go start (i + 1) (quotes lor bit)
  in
  if first < last then go first first 0

let add_type b s =
  let n = String.length s in
  let rec go start i =
    if i = n then add_text b s ~first:start ~last:i
    else
      match s.[i] with
      | ('\t' | '\n') as c ->
          add_text b s ~first:start ~last:i;
          Buffer.add_string b (if c = '\t' then "Tab\n" else "Enter\n");
          go (i + 1) (i + 1)
      | _ -> go start (i + 1)
  in
  go 0 0

let add_line b fmt = Printf.bprintf b (fmt ^^ "\n")

(* [is_empty a] is [true] iff [a] prints nothing. *)
let rec is_empty = function
  | Hidden actions -> List.for_all is_empty actions
  | Type s -> s = ""
  | Key _ | Sleep _ | Wait _ | Raw _ -> false

let rec add_action ~hidden b = function
  | Type s -> add_type b s
  | Key k -> add_line b "%s" (key_name k)
  | Sleep (Ms n) -> add_line b "Sleep %dms" n
  | Wait (timeout, w) ->
      Buffer.add_string b "Wait";
      (match w with Prompt -> () | Screen _ -> Buffer.add_string b "+Screen");
      Option.iter (fun (Ms n) -> Printf.bprintf b "@%dms" n) timeout;
      (match w with
      | Prompt -> ()
      | Screen re ->
          Buffer.add_char b ' ';
          Regexp_syntax.write b re);
      Buffer.add_char b '\n'
  | Hidden actions when hidden -> List.iter (add_action ~hidden b) actions
  | Hidden actions when List.for_all is_empty actions -> ()
  | Hidden actions ->
      Buffer.add_string b "Hide\n";
      List.iter (add_action ~hidden:true b) actions;
      Buffer.add_string b "Show\n"
  | Raw line -> add_line b "%s" line

let add_setting b s =
  Buffer.add_string b "Set ";
  (match s with
  | Shell sh -> Printf.bprintf b "Shell %S" (shell_name sh)
  | Font_size n -> Printf.bprintf b "FontSize %d" n
  | Font_family f ->
      Buffer.add_string b "FontFamily ";
      add_string b f
  | Width n -> Printf.bprintf b "Width %d" n
  | Height n -> Printf.bprintf b "Height %d" n
  | Theme t ->
      Buffer.add_string b "Theme ";
      add_string b t
  | Padding n -> Printf.bprintf b "Padding %d" n
  | Typing_speed (Ms n) -> Printf.bprintf b "TypingSpeed %dms" n
  | Framerate n -> Printf.bprintf b "Framerate %d" n);
  Buffer.add_char b '\n'

let to_string t =
  let b = Buffer.create 4096 in
  List.iter
    (fun path ->
      Buffer.add_string b "Output ";
      add_string b path;
      Buffer.add_char b '\n')
    t.outputs;
  List.iter (add_setting b) t.settings;
  List.iter (add_action ~hidden:false b) t.actions;
  Buffer.contents b

let pp ppf t = Fmt.string ppf (to_string t)

(* Running *)

type error =
  | Executable_not_found of string
  | Failed of {
      vhs : string;
      status : [ `Exited of int | `Signaled of int ];
      stderr : string;
    }
  | Io of string

let pp_error ppf = function
  | Executable_not_found exe -> Fmt.pf ppf "executable not found: %s" exe
  | Failed { vhs; status; stderr } ->
      let lines =
        String.split_on_char '\n' stderr
        |> List.map String.trim
        |> List.filter (( <> ) "")
      in
      (match status with
      | `Exited n -> Fmt.pf ppf "%s exited with code %d" vhs n
      | `Signaled n -> Fmt.pf ppf "%s was killed by signal %d" vhs n);
      if lines <> [] then Fmt.pf ppf ": %s" (String.concat " " lines)
  | Io msg -> Fmt.string ppf msg

let io_message = function
  | Eio.Process.E (Permission_denied exe) -> exe ^ ": permission denied"
  | Eio.Process.E (Executable_format_error exe) -> exe ^ ": not an executable"
  | err -> Fmt.str "%a" Eio.Exn.pp_err err

let render ?(vhs = "vhs") ~proc ~cwd t =
  let stderr = Buffer.create 256 in
  let stdin = Eio.Flow.string_source (to_string t) in
  (* Awaiting the child rather than raising on its status lets the switch
     copy all its standard error first. *)
  match
    Eio.Switch.run @@ fun sw ->
    Eio.Process.spawn ~sw proc
      ~cwd:(cwd :> Eio.Fs.dir_ty Eio.Path.t)
      ~stdin
      ~stderr:(Eio.Flow.buffer_sink stderr)
      [ vhs ]
    |> Eio.Process.await
  with
  | `Exited 0 -> Ok ()
  | status -> Error (Failed { vhs; status; stderr = Buffer.contents stderr })
  | exception Eio.Io (Eio.Process.E (Executable_not_found exe), _) ->
      Error (Executable_not_found exe)
  | exception Eio.Io (err, _) -> Error (Io (io_message err))
