val read_line : unit -> string
(** Reads one Enter-confirmed line. Raises [End_of_file] on EOF or Ctrl+D, and
    [Sys.Break] on Ctrl+C. Interactive terminals support line editing. Raises
    [Sys_error] if stdin is a terminal but stdout is redirected. *)
