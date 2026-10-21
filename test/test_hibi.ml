open Hibi

let checks = ref 0

let check name condition =
  if not condition then failwith name;
  incr checks

let get = function Ok value -> value | Error reason -> failwith reason
let is_error = function Error _ -> true | Ok _ -> false

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_directory action =
  let path = Filename.temp_file "hibi-test-" "" in
  Unix.unlink path;
  Unix.mkdir path 0o700;
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> action path)

let write path text =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel text)

let read path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fixture_time =
  Option.get (Ptime.of_date_time ((2026, 10, 21), ((18, 0, 0), 32400)))

let create ?(recorded_at = fixture_time) ?(tz_offset_s = 32400) ?(score = None)
    ?(accomplishments = None) ?(moments = None) ?(next_step = None) () =
  Record.create ~recorded_at ~tz_offset_s ~score ~accomplishments ~moments
    ~next_step

let get_record result =
  match get result with
  | Some record -> record
  | None -> failwith "expected a nonempty record"

let record =
  get_record
    (create ~score:(Some 4) ~accomplishments:(Some "相談に乗った。\"ありがとう\"と聞けた。")
       ~moments:(Some "案A \\ 案B\tを比較した") ())

let save dir record =
  let path = ref "" in
  let result = Storage.save ~dir record ~report:(fun value -> path := value) in
  Result.map (fun () -> !path) result

let test_record () =
  List.iter
    (fun score ->
      check "valid score"
        (Record.score_of_line (string_of_int score) = Ok (Some score)))
    [ 1; 2; 3; 4; 5 ];
  List.iter
    (fun line -> check "invalid score" (is_error (Record.score_of_line line)))
    [ "0"; "6"; "-1"; "+1"; "01"; "0x1"; "1.0"; "1 2"; "１"; "abc" ];
  List.iter
    (fun line ->
      check "blank score" (Record.score_of_line line = Ok None);
      check "blank text" (Record.text_of_line line = Ok None))
    [ ""; " \t\r"; "　"; "\194\160\226\128\131" ];
  check "score padding" (Record.score_of_line "　 3\t　" = Ok (Some 3));
  check "preserve nonblank text"
    (Record.text_of_line "　 日本語 \"引用\"  " = Ok (Some "　 日本語 \"引用\"  "));
  check "reject invalid UTF-8 text" (is_error (Record.text_of_line "\255"));
  check "reject invalid UTF-8 score" (is_error (Record.score_of_line "\255"));
  check "empty answers produce no record" (create () = Ok None);
  check "only next step"
    ((get_record (create ~next_step:(Some "相談") ())).next_step = Some "相談");
  let json = Record.to_json record in
  check "JSON round trip"
    (Yojson.Basic.from_string (Yojson.Basic.to_string json) = json);
  let open Yojson.Basic.Util in
  check "exact JSON fields" (List.length (to_assoc json) = 5);
  check "score is integer" (member "score" json = `Int 4);
  check "unanswered is null" (member "next_step" json = `Null);
  check "JSON Japanese and quotes"
    (member "accomplishments" json = `String (Option.get record.accomplishments));
  check "JSON backslash and tab"
    (member "moments" json = `String (Option.get record.moments));
  let epoch = Ptime.epoch in
  check "UTC timestamp"
    (get (Record.timestamp epoch ~tz_offset_s:0) = "1970-01-01T00:00:00Z");
  check "positive half-hour offset"
    (get (Record.timestamp epoch ~tz_offset_s:19800)
    = "1970-01-01T05:30:00+05:30");
  check "negative offset and previous year"
    (get (Record.timestamp epoch ~tz_offset_s:(-12600))
    = "1969-12-31T20:30:00-03:30");
  check "unknown/unrepresentable offset rejected"
    (is_error (Record.timestamp epoch ~tz_offset_s:1));
  check "out of range offset rejected"
    (is_error (Record.timestamp epoch ~tz_offset_s:86400))

let test_create () =
  for mask = 0 to 15 do
    let present bit value = if mask land bit = 0 then None else Some value in
    let score = present 1 4 in
    let accomplishments = present 2 "達成" in
    let moments = present 4 "場面" in
    let next_step = present 8 "相談" in
    match get (create ~score ~accomplishments ~moments ~next_step ()) with
    | None -> check "only all absent is empty" (mask = 0)
    | Some record ->
        check "nonempty mask" (mask <> 0);
        check "constructor preserves answers"
          (record.score = score
          && record.accomplishments = accomplishments
          && record.moments = moments
          && record.next_step = next_step);
        check "constructor preserves start time"
          (record.recorded_at = "2026-10-21T18:00:00+09:00")
  done;
  List.iter
    (fun score ->
      check "constructor rejects invalid score"
        (is_error (create ~score:(Some score) ~next_step:(Some "回答あり") ())))
    [ min_int; -1; 0; 6; 99; max_int ];
  List.iter
    (fun blank ->
      check "constructor normalizes blank answers"
        (create ~accomplishments:(Some blank) ~moments:(Some blank)
           ~next_step:(Some blank) ()
        = Ok None);
      let record =
        get_record
          (create ~score:(Some 1) ~accomplishments:(Some blank)
             ~moments:(Some blank) ~next_step:(Some blank) ())
      in
      check "blank text stays absent with a score"
        (record.accomplishments = None
        && record.moments = None && record.next_step = None))
    [ ""; " \t\r"; "　"; "\194\160\226\128\131" ];
  List.iter
    (fun score ->
      List.iter
        (fun (accomplishments, moments, next_step) ->
          check "constructor rejects invalid UTF-8 in every field"
            (is_error (create ~score ~accomplishments ~moments ~next_step ())))
        [
          (Some "\255", None, None);
          (None, Some "\255", None);
          (None, None, Some "\255");
        ])
    [ None; Some 4 ];
  let text = "　 日本語 \"引用\" \\ \n\t\000 " in
  let record = get_record (create ~moments:(Some text) ()) in
  check "constructor preserves nonblank text exactly"
    (record.moments = Some text);
  List.iter
    (fun tz_offset_s ->
      check "constructor rejects invalid offset even without answers"
        (is_error (create ~tz_offset_s ())))
    [ min_int; -86400; -1; 1; 86400; max_int ];
  let minimum =
    get_record
      (create ~recorded_at:Ptime.min ~tz_offset_s:(-60) ~score:(Some 1) ())
  in
  let maximum =
    get_record
      (create ~recorded_at:Ptime.max ~tz_offset_s:60 ~score:(Some 1) ())
  in
  check "minimum local date preserves existing UTC fallback"
    (minimum.recorded_at = "0000-01-01T00:00:00Z");
  check "maximum local date preserves existing UTC fallback"
    (maximum.recorded_at = "9999-12-31T23:59:59Z")

let test_storage () =
  with_directory (fun root ->
      let dir = Filename.concat root "one/two/logs" in
      let first = get (save dir record) in
      let original = read first in
      let second = get (save dir record) in
      check "different filenames at same timestamp" (first <> second);
      check "existing record unchanged" (read first = original);
      check "two files" (Array.length (Sys.readdir dir) = 2);
      check "saved JSON"
        (Yojson.Basic.from_string (read second) = Record.to_json record);
      check "timestamp in filename"
        (String.starts_with ~prefix:"2026-10-21T18-00-00+09-00-"
           (Filename.basename first));
      check "private new directory" ((Unix.stat dir).st_perm land 0o077 = 0);
      check "private new file" ((Unix.stat first).st_perm land 0o077 = 0);
      let collision =
        Filename.concat dir "2026-10-21T18-00-00+09-00-003.json"
      in
      write collision "preexisting";
      let fourth = get (save dir record) in
      check "preexisting collision skipped" (fourth <> collision);
      check "preexisting collision unchanged" (read collision = "preexisting");
      let file = Filename.concat root "regular-file" in
      write file "keep";
      let reported = ref false in
      check "file as directory fails"
        (is_error
           (Storage.save ~dir:file record ~report:(fun _ -> reported := true)));
      check "failure never reports success" (not !reported);
      check "failed save preserves existing file" (read file = "keep");
      let existing = Filename.concat root "existing" in
      Unix.mkdir existing 0o750;
      let permissions = (Unix.stat existing).st_perm in
      ignore (get (save existing record));
      check "existing directory permissions unchanged"
        ((Unix.stat existing).st_perm = permissions));
  with_directory (fun root ->
      let existing = Filename.concat root "target" in
      write existing "original";
      let link = Filename.concat root "2026-10-21T18-00-00+09-00-001.json" in
      Unix.symlink existing link;
      let saved = get (save root record) in
      check "collision does not follow symlink" (saved <> link);
      check "symlink target unchanged" (read existing = "original"))

let test_git_guard () =
  List.iter
    (fun marker_is_directory ->
      with_directory (fun root ->
          let repo = Filename.concat root "repo" in
          Unix.mkdir repo 0o700;
          let marker = Filename.concat repo ".git" in
          if marker_is_directory then Unix.mkdir marker 0o700
          else write marker "gitdir: dummy\n";
          let child = Filename.concat repo "new/deep" in
          check "Git directory rejected" (is_error (save child record));
          check "no directories created inside Git repo"
            (not (Sys.file_exists (Filename.concat repo "new")));
          let link = Filename.concat root "link" in
          Unix.symlink repo link;
          check "symlink into Git repo rejected"
            (is_error (save (Filename.concat link "new") record));
          let indirect = Filename.concat root "missing/../repo/new" in
          check "missing components and dot-dot rejected"
            (is_error (save indirect record));
          check "still no directories inside Git repo"
            (not (Sys.file_exists (Filename.concat repo "new")))))
    [ true; false ]

let test_bare_git_guard () =
  with_directory (fun root ->
      let repo = Filename.concat root "bare.git" in
      Unix.mkdir repo 0o700;
      let head = Filename.concat repo "HEAD" in
      write head "ref: refs/heads/main\n";
      Unix.mkdir (Filename.concat repo "objects") 0o700;
      Unix.mkdir (Filename.concat repo "refs") 0o700;
      let link = Filename.concat root "link" in
      Unix.symlink repo link;
      let original =
        Array.to_list (Sys.readdir repo) |> List.sort String.compare
      in
      List.iter
        (fun dir ->
          let reported = ref false in
          check "bare repository rejected"
            (is_error
               (Storage.save ~dir record ~report:(fun _ -> reported := true)));
          check "bare rejection never reports success" (not !reported);
          check "bare repository unchanged"
            (Array.to_list (Sys.readdir repo)
            |> List.sort String.compare = original);
          check "bare HEAD unchanged" (read head = "ref: refs/heads/main\n"))
        [
          repo;
          Filename.concat repo "objects";
          Filename.concat repo "new/deep";
          Filename.concat link "new";
        ];
      let ordinary = Filename.concat root "ordinary" in
      Unix.mkdir ordinary 0o700;
      write (Filename.concat ordinary "HEAD") "ordinary file";
      check "HEAD alone does not reject an ordinary directory"
        (Result.is_ok (save ordinary record)))

let test_cleanup () =
  with_directory (fun dir ->
      let result =
        Storage.save ~dir record ~report:(fun _ ->
            raise (Sys_error "test report failure"))
      in
      check "report failure is error" (is_error result);
      check "failed report removes this record" (Sys.readdir dir = [||]);
      let original_mask = Unix.sigprocmask Unix.SIG_BLOCK [] in
      (try
         ignore (Storage.save ~dir record ~report:(fun _ -> raise Sys.Break));
         failwith "expected cancellation"
       with Sys.Break -> ());
      check "cancellation after close removes this record"
        (Sys.readdir dir = [||]);
      check "signal mask restored"
        (Unix.sigprocmask Unix.SIG_BLOCK [] = original_mask);
      let existing = get (save dir record) in
      let contents = read existing in
      (try ignore (Storage.save ~dir record ~report:(fun _ -> raise Sys.Break))
       with Sys.Break -> ());
      check "cancellation preserves older record"
        (Array.length (Sys.readdir dir) = 1 && read existing = contents))

let () =
  test_record ();
  test_create ();
  test_storage ();
  test_git_guard ();
  test_bare_git_guard ();
  test_cleanup ();
  Printf.printf "hibi: %d checks passed\n" !checks
