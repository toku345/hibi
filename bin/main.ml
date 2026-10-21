open Hibi

let ( let* ) = Result.bind

let ask question notes parse =
  print_endline question;
  List.iter print_endline notes;
  let rec answer () =
    match parse (Input.read_line ()) with
    | Ok value -> value
    | Error reason ->
        print_endline reason;
        answer ()
  in
  let value = answer () in
  print_newline ();
  value

let started_at () =
  let time = Ptime_clock.now () in
  match Ptime_clock.current_tz_offset_s () with
  | None -> Error "ローカル日時のオフセットを取得できません。"
  | Some tz_offset_s -> Record.timestamp time ~tz_offset_s

let collect_record () =
  let* recorded_at = started_at () in
  let score =
    ask "今日の仕事を終えて、気持ちは？（1〜5）"
      [ "1＝とても消耗した、3＝どちらともいえない、5＝とても充実した"; "成果や能力の自己採点ではありません。" ]
      Record.score_of_line
  in
  let accomplishments =
    ask "今日やれたこと・頑張ったことは？" [ "小さな貢献や、踏ん張ったことでもOK。" ] Record.text_of_line
  in
  let moments =
    ask "今日、心が動いた場面は？"
      [ "うれしい・面白い・モヤモヤなど、どんな感情でもOK。"; "余裕があれば「何が起きたか → どう感じたか」を一言ずつ。" ]
      Record.text_of_line
  in
  let next_step =
    ask "明日、少し変えてみたいことは？" [ "あればでOK。小さな相談や試みでも十分。" ] Record.text_of_line
  in
  Ok Record.{ recorded_at; score; accomplishments; moments; next_step }

let run () =
  let* record = collect_record () in
  if Record.is_empty record then (
    print_endline "全問未回答のため、保存しませんでした。";
    Ok ())
  else
    let* dir = Storage.log_directory () in
    Storage.save ~dir record ~report:(fun path ->
        Printf.printf "保存しました: %s\n%!" path)

let () =
  Sys.catch_break true;
  let outcome =
    try run () with
    | End_of_file -> Error "入力が終了したため、保存しませんでした。"
    | Sys_error reason -> Error reason
    | Unix.Unix_error (error, _, path) ->
        Error (Printf.sprintf "%s: %s" path (Unix.error_message error))
    | Sys.Break ->
        prerr_endline "\n中断しました。保存していません。";
        exit 130
  in
  match outcome with
  | Ok () -> ()
  | Error reason ->
      prerr_endline ("エラー: " ^ reason);
      exit 1
