type t = {
  recorded_at : string;
  score : int option;
  accomplishments : string option;
  moments : string option;
  next_step : string option;
}

let timestamp time ~tz_offset_s =
  if tz_offset_s mod 60 <> 0 || tz_offset_s < -86340 || tz_offset_s > 86340 then
    Error "ローカル日時のオフセットを表現できません。"
  else
    let value = Ptime.to_rfc3339 ~tz_offset_s time in
    if String.ends_with ~suffix:"-00:00" value then Error "ローカル日時を表現できません。"
    else Ok value

let is_space uchar =
  match Uchar.to_int uchar with
  | 0x0020 | 0x0085 | 0x00a0 | 0x1680 | 0x2028 | 0x2029 | 0x202f | 0x205f
  | 0x3000 ->
      true
  | code
    when (code >= 0x0009 && code <= 0x000d) || (code >= 0x2000 && code <= 0x200a)
    ->
      true
  | _ -> false

let trimmed line =
  if not (String.is_valid_utf_8 line) then Error "UTF-8の文字列を入力してください。"
  else
    let rec scan index first last =
      if index = String.length line then
        Ok (String.sub line first (last - first))
      else
        let decoded = String.get_utf_8_uchar line index in
        let next = index + Uchar.utf_decode_length decoded in
        if is_space (Uchar.utf_decode_uchar decoded) then scan next first last
        else scan next (if last = 0 then index else first) next
    in
    scan 0 0 0

let score_of_line line =
  match trimmed line with
  | Error _ as error -> error
  | Ok "" -> Ok None
  | Ok (("1" | "2" | "3" | "4" | "5") as score) ->
      Ok (Some (int_of_string score))
  | Ok _ -> Error "1〜5の整数を入力してください（空Enterでスキップ）。"

let text_of_line line =
  match trimmed line with
  | Error _ as error -> error
  | Ok "" -> Ok None
  | Ok _ -> Ok (Some line)

let create ~recorded_at ~tz_offset_s ~score ~accomplishments ~moments ~next_step
    =
  let ( let* ) = Result.bind in
  let* recorded_at = timestamp recorded_at ~tz_offset_s in
  let* score =
    match score with
    | None -> Ok None
    | Some value when value >= 1 && value <= 5 -> Ok score
    | Some _ -> Error "1〜5の整数を入力してください（空Enterでスキップ）。"
  in
  let text = function None -> Ok None | Some line -> text_of_line line in
  let* accomplishments = text accomplishments in
  let* moments = text moments in
  let* next_step = text next_step in
  if
    score = None && accomplishments = None && moments = None && next_step = None
  then Ok None
  else Ok (Some { recorded_at; score; accomplishments; moments; next_step })

let to_json record =
  let text = function None -> `Null | Some value -> `String value in
  `Assoc
    [
      ("recorded_at", `String record.recorded_at);
      ( "score",
        match record.score with None -> `Null | Some value -> `Int value );
      ("accomplishments", text record.accomplishments);
      ("moments", text record.moments);
      ("next_step", text record.next_step);
    ]
