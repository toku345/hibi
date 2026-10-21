(* input_line also accepts a final unterminated line. Only Enter confirms an
   answer here; EOF discards even a partially typed final answer. *)
let read_pipe () =
  let buffer = Buffer.create 80 in
  let rec read () =
    match input_char stdin with
    | '\n' ->
        let line = Buffer.contents buffer in
        let length = String.length line in
        if length > 0 && line.[length - 1] = '\r' then
          String.sub line 0 (length - 1)
        else line
    | character ->
        Buffer.add_char buffer character;
        read ()
  in
  print_string "> ";
  flush stdout;
  read ()

let read_terminal () =
  let terminal = Lwt_main.run (Lazy.force LTerm.stdout) in
  let reader =
    object
      inherit LTerm_read_line.read_line ()
      inherit [Zed_string.t] LTerm_read_line.term terminal as super
      method! show_box = false

      method! private exec ?keys actions =
        (* EOF cancels even a partial answer. Do not let the editor create
           extra files containing private answers or insert extra lines. *)
        if List.mem LTerm_read_line.Interrupt_or_delete_next_char actions then
          Lwt.fail End_of_file
        else
          let actions =
            List.filter
              (function
                | LTerm_read_line.Edit_with_external_editor
                | LTerm_read_line.Edit (LTerm_edit.Zed Zed_edit.Newline) ->
                    false
                | _ -> true)
              actions
          in
          super#exec ?keys actions
    end
  in
  reader#set_prompt (React.S.const (LTerm_text.of_utf8 "> "));
  let handler =
    Lwt_unix.on_signal Sys.sigint (fun _ ->
        if Lwt_mvar.is_empty reader#interrupt then
          ignore (Lwt_mvar.put reader#interrupt Sys.Break))
  in
  Fun.protect
    ~finally:(fun () ->
      Lwt_unix.disable_signal_handler handler;
      Sys.catch_break true)
    (fun () ->
      try
        let line =
          Lwt_main.run
            (Lwt.bind reader#run (fun line ->
                 Lwt.map (fun () -> line) (LTerm.flush terminal)))
        in
        Zed_string.to_utf8 line
      with LTerm_read_line.Interrupt -> raise End_of_file)

let read_line () =
  flush stdout;
  if Unix.isatty Unix.stdin && Unix.isatty Unix.stdout then read_terminal ()
  else read_pipe ()
