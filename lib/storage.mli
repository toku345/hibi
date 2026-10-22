val log_directory : unit -> (string, string) result

val save :
  dir:string -> Entry.t -> report:(string -> unit) -> (unit, string) result
(** [save ~dir entry ~report] writes a new entry and reports its path. Failure
    or cancellation before [report] returns removes this new file. [report]
    should finish by flushing its output. *)
