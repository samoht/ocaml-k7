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

(* VHS lexes a bare [m] or [s] as a time unit; [Ctrl] takes them quoted, but
   [Alt] then presses nothing. *)
let check_key = function
  | Ctrl c -> check_chord "Ctrl" {|@[\]^-.|} c
  | Alt (('m' | 's') as c) -> err "Alt+%c: VHS cannot press it" c
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

(* Go's [filepath.Ext]: the suffix from the last dot of the last segment. *)
let extension path =
  match String.rindex_opt path '.' with
  | Some dot when not (String.contains_from path dot '/') ->
      Some (String.sub path dot (String.length path - dot))
  | _ -> None

(* VHS hands an unknown extension to ffmpeg as a GIF, and ffmpeg then picks
   the format from the extension and fails. *)
let extensions = [ ".gif"; ".mp4"; ".webm"; ".txt"; ".ascii"; ".test"; ".png" ]

let check_output path =
  let* () = check_quotable "Output" path in
  if String.starts_with ~prefix:"-" path then
    err "Output %S: starts with -, which ffmpeg reads as an option" path
  else if String.ends_with ~suffix:"/" path then Ok ()
  else
    match extension path with
    | None -> err "Output %S: neither an extension nor a trailing slash" path
    | Some e when List.mem e extensions -> Ok ()
    | Some _ ->
        err "Output %S: not .gif, .mp4, .webm, .txt, .ascii, .test or .png" path

(* VHS fonts are set by browser code that quotes the list with [']. *)
let check_setting = function
  | Shell _ -> Ok ()
  | Font_size n when n <= 0 -> err "Set FontSize %d: not positive" n
  | Framerate n when n <= 0 -> err "Set Framerate %d: not positive" n
  | Padding n when n < 0 -> err "Set Padding %d: negative" n
  | Font_family f when String.exists (fun c -> c = '\'' || c = '\\') f ->
      err "Set FontFamily %S: holds a single quote or a backslash" f
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
  | Ctrl (('m' | 's') as c) -> "Ctrl+\"" ^ String.make 1 c ^ "\""
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

let add_line b s =
  Buffer.add_string b s;
  Buffer.add_char b '\n'

let add_ms b n =
  Buffer.add_string b (Int.to_string n);
  Buffer.add_string b "ms"

(* [is_empty a] is [true] iff [a] prints nothing. *)
let rec is_empty = function
  | Hidden actions -> List.for_all is_empty actions
  | Type s -> s = ""
  | Key _ | Sleep _ | Wait _ | Raw _ -> false

let rec add_action ~hidden b = function
  | Type s -> add_type b s
  | Key k -> add_line b (key_name k)
  | Sleep (Ms n) ->
      Buffer.add_string b "Sleep ";
      add_ms b n;
      Buffer.add_char b '\n'
  | Wait (timeout, w) ->
      Buffer.add_string b "Wait";
      (match w with Prompt -> () | Screen _ -> Buffer.add_string b "+Screen");
      Option.iter
        (fun (Ms n) ->
          Buffer.add_char b '@';
          add_ms b n)
        timeout;
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
  | Raw line -> add_line b line

let setting_line = function
  | Shell sh -> ("Shell", "\"" ^ shell_name sh ^ "\"")
  | Font_size n -> ("FontSize", Int.to_string n)
  | Font_family f -> ("FontFamily", f)
  | Width n -> ("Width", Int.to_string n)
  | Height n -> ("Height", Int.to_string n)
  | Theme t -> ("Theme", t)
  | Padding n -> ("Padding", Int.to_string n)
  | Typing_speed (Ms n) -> ("TypingSpeed", Int.to_string n ^ "ms")
  | Framerate n -> ("Framerate", Int.to_string n)

let add_setting b s =
  let name, value = setting_line s in
  Buffer.add_string b "Set ";
  Buffer.add_string b name;
  Buffer.add_char b ' ';
  (match s with
  | Font_family _ | Theme _ -> add_string b value
  | _ -> Buffer.add_string b value);
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
      output : string;
    }
  | Io of string

(* VHS styles its messages with ANSI escapes when it thinks it can. *)
let strip_ansi s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i < n then
      if s.[i] = '\027' && i + 1 < n && s.[i + 1] = '[' then skip (i + 2)
      else (
        Buffer.add_char b s.[i];
        go (i + 1))
  and skip i =
    if i < n then match s.[i] with '@' .. '~' -> go (i + 1) | _ -> skip (i + 1)
  in
  go 0;
  Buffer.contents b

let last_lines = 10

let pp_error ppf = function
  | Executable_not_found exe -> Fmt.pf ppf "executable not found: %s" exe
  | Failed { vhs; status; output } ->
      (match status with
      | `Exited n -> Fmt.pf ppf "%s exited with code %d" vhs n
      | `Signaled n -> Fmt.pf ppf "%s was killed by signal %d" vhs n);
      let lines =
        String.split_on_char '\n' (strip_ansi output)
        |> List.map String.trim
        |> List.filter (( <> ) "")
      in
      let drop = List.length lines - last_lines in
      List.iteri (fun i l -> if i >= drop then Fmt.pf ppf "@\n  %s" l) lines
  | Io msg -> Fmt.string ppf msg

let fs_message path = function
  | Eio.Fs.Not_found _ -> path ^ ": no such file or directory"
  | Eio.Fs.Permission_denied _ -> path ^ ": permission denied"
  | Eio.Fs.Already_exists _ -> path ^ ": already exists"
  | Eio.Fs.Not_native msg -> path ^ ": " ^ msg
  | e -> Fmt.str "%s: %a" path Eio.Exn.pp_err (Eio.Fs.E e)

let io_message ~cwd = function
  | Eio.Process.E (Permission_denied exe) -> exe ^ ": permission denied"
  | Eio.Process.E (Executable_format_error exe) -> exe ^ ": not an executable"
  | Eio.Fs.E e ->
      fs_message (Option.value (Eio.Path.native cwd) ~default:(snd cwd)) e
  | err -> Fmt.str "%a" Eio.Exn.pp_err err

(* [run_vhs] runs protected from cancellation, so that its switch keeps
   reaping [vhs] until it exits; [cancelled] makes it send [vhs] SIGTERM,
   which [vhs] cleans up after. *)
let run_vhs ~vhs ~proc ~cwd ~cancelled ~sink tape =
  Eio.Switch.run @@ fun sw ->
  let stdin, feed = Eio.Process.pipe ~sw proc in
  let child =
    Eio.Process.spawn ~sw proc
      ~cwd:(cwd :> Eio.Fs.dir_ty Eio.Path.t)
      ~stdin ~stdout:sink ~stderr:sink [ vhs; "-q" ]
  in
  Eio.Flow.close stdin;
  (* A [vhs] that exits before reading the whole tape reports why itself. *)
  Eio.Fiber.fork ~sw (fun () ->
      (try Eio.Flow.copy_string tape feed with Eio.Io _ -> ());
      Eio.Flow.close feed);
  Eio.Fiber.first
    (fun () -> Eio.Process.await child)
    (fun () ->
      Eio.Promise.await cancelled;
      Eio.Process.signal child Sys.sigterm;
      Eio.Process.await child)

let render ?(vhs = "vhs") ~proc ~cwd t =
  let output = Buffer.create 256 in
  let sink = Eio.Flow.buffer_sink output in
  let tape = to_string t in
  let cancelled, cancel = Eio.Promise.create () in
  let finished, finish = Eio.Promise.create () in
  let supervise () =
    Eio.Cancel.protect @@ fun () ->
    Eio.Promise.resolve finish
      (match run_vhs ~vhs ~proc ~cwd ~cancelled ~sink tape with
      | status -> Ok status
      | exception (Eio.Io _ as e) -> Error e)
  in
  let wait () =
    match Eio.Promise.await finished with
    | outcome -> outcome
    | exception (Eio.Cancel.Cancelled _ as e) ->
        Eio.Promise.resolve cancel ();
        raise e
  in
  match
    Eio.Switch.run @@ fun sw ->
    Eio.Fiber.fork ~sw supervise;
    wait ()
  with
  | Ok (`Exited 0) -> Ok ()
  | Ok status -> Error (Failed { vhs; status; output = Buffer.contents output })
  | Error (Eio.Io (Eio.Process.E (Executable_not_found exe), _)) ->
      Error (Executable_not_found exe)
  | Error (Eio.Io (err, _)) -> Error (Io (io_message ~cwd err))
  | Error e -> raise e
