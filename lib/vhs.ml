module Value = Value
module Cram = Cram

type duration = Value.duration = Ms of int | S of float

type key = Value.key =
  | Enter
  | Tab
  | Space
  | Backspace
  | Escape
  | Left
  | Right
  | Up
  | Down
  | Ctrl of string
  | Alt of string
  | Ctrl_alt of string
  | Raw_key of string

type setting = Value.setting =
  | Shell of string
  | Font_size of int
  | Font_family of string
  | Width of int
  | Height of int
  | Theme of string
  | Padding of int
  | Typing_speed of duration
  | Framerate of int
  | Custom of string * string

type wait = Value.wait =
  | Wait of string
  | Wait_screen of string
  | Wait_line of string

type command = Value.command =
  | Type of string
  | Key of key
  | Sleep of duration
  | Wait_for of wait
  | Hide
  | Show
  | Screenshot of string option
  | Source of string
  | Raw_command of string

type item = Value.item =
  | Comment of string
  | Blank
  | Require of string
  | Output of string
  | Env of string * string
  | Set of setting
  | Command of command
  | Raw_line of string

type t = Value.t

let tape = Value.tape
let items = Value.items
let comment = Value.comment
let blank = Value.blank
let require = Value.require
let output = Value.output
let env = Value.env
let set = Value.set
let command = Value.command
let raw_line = Value.raw_line
let type' = Value.type'
let key = Value.key
let enter = Value.enter
let sleep = Value.sleep
let wait = Value.wait
let hide = Value.hide
let show = Value.show
let screenshot = Value.screenshot
let source = Value.source
let raw_command = Value.raw_command
let pp = Value.pp
let to_string = Value.to_string

type error =
  | Executable_not_found of string
  | Child_error of { argv : string list; status : Eio.Process.exit_status }
  | Io of exn

let pp_error ppf = function
  | Executable_not_found exe -> Fmt.pf ppf "executable not found: %s" exe
  | Child_error { argv; status } ->
      Fmt.pf ppf "%a failed: %a" Eio.Process.pp_args argv Eio.Process.pp_status
        status
  | Io (Sys_error msg) -> Fmt.string ppf msg
  | Io exn -> Fmt.exn ppf exn

let write_file ~path t =
  let contents = to_string t in
  try Ok (Out_channel.with_open_bin path (fun oc -> output_string oc contents))
  with Sys_error _ as exn -> Error (Io exn)

let run_process proc argv =
  try Ok (Eio.Process.run proc argv) with
  | Eio.Exn.Io (Eio.Process.E (Executable_not_found exe), _) ->
      Error (Executable_not_found exe)
  | Eio.Exn.Io (Eio.Process.E (Child_error status), _) ->
      Error (Child_error { argv; status })
  | Eio.Exn.Io _ as exn -> Error (Io exn)

let parse_process proc argv =
  try Ok (Eio.Process.parse_out proc Eio.Buf_read.take_all argv) with
  | Eio.Exn.Io (Eio.Process.E (Executable_not_found exe), _) ->
      Error (Executable_not_found exe)
  | Eio.Exn.Io (Eio.Process.E (Child_error status), _) ->
      Error (Child_error { argv; status })
  | Eio.Exn.Io _ as exn -> Error (Io exn)

(* vhs reads an argument starting with '-' as a flag. *)
let path_arg path =
  if String.starts_with ~prefix:"-" path then "./" ^ path else path

let render_file ~proc ?(vhs = "vhs") path =
  run_process proc [ vhs; path_arg path ]

let render ~proc ?vhs ~path tape =
  Result.bind (write_file ~path tape) (fun () -> render_file ~proc ?vhs path)

let publish ~proc ?(vhs = "vhs") file =
  parse_process proc [ vhs; "publish"; path_arg file ]
