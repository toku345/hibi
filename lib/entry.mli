type t = private {
  recorded_at : string;
  score : int option;
  accomplishments : string option;
  moments : string option;
  next_step : string option;
}
(** 検証済みで、回答が1つ以上ある記録。フィールドは読み取れるが、外部から直接生成・更新できない。 *)

val create :
  recorded_at:Ptime.t ->
  tz_offset_s:int ->
  score:int option ->
  accomplishments:string option ->
  moments:string option ->
  next_step:string option ->
  (t option, string) result
(** 全項目を検証する。空白だけの回答は [None] にし、非空の回答は元の文字列を保持する。 全問未回答なら [Ok None]、保存可能なら
    [Ok (Some entry)]、不正値なら [Error reason] を返す。 *)

val timestamp : Ptime.t -> tz_offset_s:int -> (string, string) result
val score_of_line : string -> (int option, string) result
val text_of_line : string -> (string option, string) result
val to_json : t -> Yojson.Basic.t
