open Hibi
open QCheck2

let fixture_time = Ptime.epoch

let create ?(recorded_at = fixture_time) ?(tz_offset_s = 0)
    (score, accomplishments, moments, next_step) =
  Record.create ~recorded_at ~tz_offset_s ~score ~accomplishments ~moments
    ~next_step

let unicode =
  let scalar =
    Gen.oneof [ Gen.int_range 0 0xd7ff; Gen.int_range 0xe000 0x10ffff ]
  in
  Gen.map
    (fun scalars ->
      let buffer = Buffer.create 128 in
      List.iter
        (fun scalar -> Buffer.add_utf_8_uchar buffer (Uchar.of_int scalar))
        scalars;
      Buffer.contents buffer)
    (Gen.list_size (Gen.int_range 0 32) scalar)

let nonblank_text = Gen.map (fun text -> " \tあ\"\\\n\000" ^ text ^ "　") unicode

let valid_answers =
  Gen.map5
    (fun mask score accomplishments moments next_step ->
      let present bit value = if mask land bit = 0 then None else Some value in
      ( present 1 score,
        present 2 accomplishments,
        present 4 moments,
        present 8 next_step ))
    (Gen.int_range 1 15) (Gen.int_range 1 5) nonblank_text nonblank_text
    nonblank_text

let print_answers (score, accomplishments, moments, next_step) =
  Printf.sprintf "score=%s; accomplishments=%s; moments=%s; next_step=%s"
    (Print.option Print.int score)
    (Print.option Print.string accomplishments)
    (Print.option Print.string moments)
    (Print.option Print.string next_step)

let property name gen print = Test.make ~name ~count:1000 ~print gen

let valid_record =
  property "create preserves valid nonempty answers" valid_answers print_answers
    (fun ((score, accomplishments, moments, next_step) as answers) ->
      match create answers with
      | Ok (Some record) ->
          record.score = score
          && record.accomplishments = accomplishments
          && record.moments = moments
          && record.next_step = next_step
          && (match record.score with
            | None -> true
            | Some n -> n >= 1 && n <= 5)
          && (record.score <> None
             || record.accomplishments <> None
             || record.moments <> None || record.next_step <> None)
      | Ok None | Error _ -> false)

let blank_answers =
  let blank =
    Gen.map (String.concat "")
      (Gen.list_size (Gen.int_range 0 32)
         (Gen.oneof_list [ " "; "\t"; "\r"; "\n"; "　"; "\194\160" ]))
  in
  Gen.map3
    (fun accomplishments moments next_step ->
      (None, accomplishments, moments, next_step))
    (Gen.option blank) (Gen.option blank) (Gen.option blank)

let empty_record =
  property "absent or blank answers produce no record" blank_answers
    print_answers (fun answers -> create answers = Ok None)

let blank_normalization =
  property "blank text becomes absent even with a score" blank_answers
    print_answers (fun (_, accomplishments, moments, next_step) ->
      match create (Some 4, accomplishments, moments, next_step) with
      | Ok (Some record) ->
          record.score = Some 4
          && record.accomplishments = None
          && record.moments = None && record.next_step = None
      | Ok None | Error _ -> false)

(* 反例を縮小しても不正性を保つ。スコアの区間は1〜5を含まず、UTF-8はmapで必ず不正なバイトを付加する。 *)
let invalid_score =
  property "out-of-range scores are rejected"
    (Gen.oneof [ Gen.int_range (-1_000_000) 0; Gen.int_range 6 1_000_000 ])
    Print.int
    (fun score ->
      Result.is_error (create (Some score, None, None, Some "回答あり")))

let invalid_utf8 =
  property "invalid UTF-8 is rejected in every text field"
    (Gen.pair
       (Gen.map
          (fun bytes -> bytes ^ "\255")
          (Gen.string_size (Gen.int_range 0 128)))
       (Gen.int_range 0 2))
    (Print.pair Print.string Print.int)
    (fun (text, field) ->
      let at n = if field = n then Some text else Some "回答あり" in
      Result.is_error (create (Some 4, at 0, at 1, at 2)))

let json_round_trip =
  property "JSON round trip preserves all answer values" valid_answers
    print_answers (fun (score, accomplishments, moments, next_step) ->
      match create (score, accomplishments, moments, next_step) with
      | Error _ | Ok None -> false
      | Ok (Some record) ->
          let json =
            Yojson.Basic.from_string
              (Yojson.Basic.pretty_to_string (Record.to_json record))
          in
          let text = function None -> `Null | Some value -> `String value in
          json
          = `Assoc
              [
                ("recorded_at", `String "1970-01-01T00:00:00Z");
                ( "score",
                  match score with None -> `Null | Some value -> `Int value );
                ("accomplishments", text accomplishments);
                ("moments", text moments);
                ("next_step", text next_step);
              ])

let timestamps =
  Gen.pair
    (Gen.int_range (-1_000_000_000) 1_000_000_000)
    (Gen.pair (Gen.int_range 0 999) (Gen.int_range (-1439) 1439))

let timestamp_round_trip =
  property "timestamp preserves the captured instant and offset" timestamps
    (Print.pair Print.int (Print.pair Print.int Print.int))
    (fun (seconds, (milliseconds, minutes)) ->
      let recorded_at =
        Option.get
          (Ptime.of_float_s
             (float_of_int seconds +. (float_of_int milliseconds /. 1000.)))
      in
      let tz_offset_s = minutes * 60 in
      match create ~recorded_at ~tz_offset_s (Some 3, None, None, None) with
      | Error _ | Ok None -> false
      | Ok (Some record) -> (
          match Ptime.of_rfc3339 ~strict:true record.recorded_at with
          | Ok (time, Some offset, count) ->
              Ptime.equal time (Ptime.truncate ~frac_s:0 recorded_at)
              && offset = tz_offset_s
              && count = String.length record.recorded_at
          | Ok (_, None, _) | Error _ -> false))

let invalid_offset =
  property "invalid offsets are rejected even without answers"
    (Gen.oneof
       [
         Gen.map
           (fun minutes -> (minutes * 60) + 1)
           (Gen.int_range (-1000) 1000);
         Gen.int_range (-1_000_000) (-86400);
         Gen.int_range 86400 1_000_000;
       ])
    Print.int
    (fun tz_offset_s ->
      Result.is_error (create ~tz_offset_s (None, None, None, None)))

let () =
  QCheck_base_runner.run_tests_main
    [
      valid_record;
      empty_record;
      blank_normalization;
      invalid_score;
      invalid_utf8;
      json_round_trip;
      timestamp_round_trip;
      invalid_offset;
    ]
