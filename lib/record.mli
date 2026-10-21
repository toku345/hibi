type t = {
  recorded_at : string;
  score : int option;
  accomplishments : string option;
  moments : string option;
  next_step : string option;
}

val timestamp : Ptime.t -> tz_offset_s:int -> (string, string) result
val score_of_line : string -> (int option, string) result
val text_of_line : string -> (string option, string) result
val is_empty : t -> bool
val to_json : t -> Yojson.Basic.t
