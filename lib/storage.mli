val log_directory : unit -> (string, string) result

val save :
  dir:string -> Record.t -> report:(string -> unit) -> (unit, string) result
(** [save ~dir record ~report] writes a new record and reports its path. Failure
    or cancellation before [report] returns removes this new file. [report]
    should finish by flushing its output. *)
