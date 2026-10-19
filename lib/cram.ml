type step = {
  command : string list;
  output : string list;
  exit_code : int option;
}

type t = step list

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.equal (String.sub s 0 (String.length prefix)) prefix

let drop n s = String.sub s n (String.length s - n)

(* [  [N]] exit-code annotation: two-space indent, then [N] on its own. *)
let exit_code_of_output line =
  if
    String.length line >= 3
    && Char.equal line.[0] '['
    && Char.equal line.[String.length line - 1] ']'
  then int_of_string_opt (String.sub line 1 (String.length line - 2))
  else None

let parse src =
  let lines = String.split_on_char '\n' src in
  let steps = ref [] in
  let cmd = ref [] and out = ref [] and exit = ref None and open_ = ref false in
  let flush () =
    if !open_ then
      steps :=
        { command = List.rev !cmd; output = List.rev !out; exit_code = !exit }
        :: !steps;
    cmd := [];
    out := [];
    exit := None;
    open_ := false
  in
  List.iter
    (fun line ->
      if starts_with ~prefix:"  $ " line then begin
        flush ();
        open_ := true;
        cmd := [ drop 4 line ]
      end
      else if !open_ && starts_with ~prefix:"  > " line then
        cmd := drop 4 line :: !cmd
      else if !open_ && starts_with ~prefix:"  " line then begin
        let content = drop 2 line in
        match exit_code_of_output content with
        | Some _ as code -> exit := code
        | None -> out := content :: !out
      end
      else flush ())
    lines;
  flush ();
  List.rev !steps

let regexp_meta = function
  | '\\' | '.' | '+' | '*' | '?' | '(' | ')' | '[' | ']' | '{' | '}' | '^' | '$'
  | '|' ->
      true
  | _ -> false

let escape_regexp s =
  let b = Buffer.create (String.length s + 8) in
  String.iter
    (fun c ->
      if regexp_meta c then Buffer.add_char b '\\';
      Buffer.add_char b c)
    s;
  Buffer.contents b

(* The last non-empty output line is the one nearest the returned prompt, so
   it is the safest "everything has rendered" marker to wait on. *)
let wait_pattern output =
  let nonempty =
    List.filter (fun l -> not (String.equal (String.trim l) "")) output
  in
  match List.rev nonempty with
  | last :: _ -> Some (escape_regexp (String.trim last))
  | [] -> None

(* Source the setup scripts, then clear, all hidden: the [clear] must come
   after the [source] lines so their echoes are wiped from the scrollback
   before [Show] resumes capture, otherwise they leak into the recording. *)
let setup_block = function
  | [] -> []
  | scripts ->
      let open Value in
      hide ()
      :: List.concat_map
           (fun s -> [ type' ("source " ^ Filename.quote s); enter () ])
           scripts
      @ [ type' "clear"; enter (); show () ]

let step_items ~pause step =
  let open Value in
  List.concat_map (fun line -> [ type' line; enter () ]) step.command
  @ (match wait_pattern step.output with
    | Some pat -> [ wait (Wait_screen pat) ]
    | None -> [])
  @ [ sleep pause ]

let to_tape ?(settings = []) ?(setup_scripts = []) ?(pause = Value.S 1.)
    ?(final_pause = Value.S 3.) ~output (steps : t) =
  let last = List.length steps - 1 in
  let items =
    (Value.output output :: List.map Value.set settings)
    @ setup_block setup_scripts
    @ List.concat
        (List.mapi
           (fun index step ->
             let pause = if index = last then final_pause else pause in
             step_items ~pause step)
           steps)
  in
  Value.tape items

let pp ppf steps =
  List.iter
    (fun { command; output; exit_code } ->
      List.iter (Fmt.pf ppf "$ %s@\n") command;
      List.iter (Fmt.pf ppf "%s@\n") output;
      Option.iter (Fmt.pf ppf "[%d]@\n") exit_code)
    steps
