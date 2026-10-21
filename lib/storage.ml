let ( let* ) = Result.bind

let log_directory () =
  match Sys.getenv_opt "HIBI_LOG_DIR" with
  | Some "" -> Error "HIBI_LOG_DIRが空です。"
  | Some dir -> Ok dir
  | None -> (
      match Sys.getenv_opt "HOME" with
      | None | Some "" -> Error "HOMEが不明です。HIBI_LOG_DIRを指定してください。"
      | Some home -> Ok (Filename.concat home ".local/share/hibi"))

let unix_error error operation path =
  Printf.sprintf "%s (%s): %s" path operation (Unix.error_message error)

let has_kind path kind =
  try (Unix.stat path).st_kind = kind
  with Unix.Unix_error (Unix.ENOENT, _, _) -> false

let is_bare_repository dir =
  has_kind (Filename.concat dir "HEAD") Unix.S_REG
  && has_kind (Filename.concat dir "objects") Unix.S_DIR
  && has_kind (Filename.concat dir "refs") Unix.S_DIR

let rec outside_git dir =
  match Unix.lstat (Filename.concat dir ".git") with
  | _ -> Error ("Gitリポジトリの外を保存先に指定してください: " ^ dir)
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      if is_bare_repository dir then Error ("Gitリポジトリの外を保存先に指定してください: " ^ dir)
      else
        let parent = Filename.dirname dir in
        if parent = dir then Ok () else outside_git parent

let checked_directory path =
  let dir = Unix.realpath path in
  if (Unix.stat dir).st_kind <> Unix.S_DIR then Error ("ディレクトリではありません: " ^ dir)
  else
    let* () = outside_git dir in
    Ok dir

let rec prepare_directory path =
  match Unix.stat path with
  | _ -> checked_directory path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      let parent = Filename.dirname path in
      if parent = path then Error ("保存先を作成できません: " ^ path)
      else
        let* parent = prepare_directory parent in
        let* () = outside_git parent in
        let child = Filename.concat parent (Filename.basename path) in
        (try Unix.mkdir child 0o700
         with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
        checked_directory child

(* Defer SIGINT only while acquiring or releasing the file, so the cleanup
   always knows which file and descriptor belong to this invocation. *)
let with_sigint_blocked action =
  let previous = Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigint ] in
  match action () with
  | value ->
      ignore (Unix.sigprocmask Unix.SIG_SETMASK previous);
      value
  | exception exn ->
      ignore (Unix.sigprocmask Unix.SIG_SETMASK previous);
      raise exn

let save ~dir record ~report =
  let created = ref None in
  let output = ref None in
  let cleanup () =
    with_sigint_blocked (fun () ->
        match !created with
        | None -> Ok ()
        | Some (path, fd) -> (
            (match !output with
            | Some channel -> close_out_noerr channel
            | None -> ( try Unix.close fd with Unix.Unix_error _ -> ()));
            try
              Unix.unlink path;
              created := None;
              Ok ()
            with Unix.Unix_error (error, operation, path) ->
              Error (unix_error error operation path)))
  in
  let fail reason =
    match cleanup () with
    | Ok () -> Error reason
    | Error cleanup_error ->
        Error (reason ^ " / 作成したファイルの削除にも失敗しました: " ^ cleanup_error)
  in
  try
    let json = Yojson.Basic.pretty_to_string (Record.to_json record) ^ "\n" in
    let* dir = prepare_directory dir in
    let* dir = checked_directory dir in
    let stem = String.map (function ':' -> '-' | c -> c) record.recorded_at in
    let rec create suffix =
      let path =
        Filename.concat dir (Printf.sprintf "%s-%03d.json" stem suffix)
      in
      match
        Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] 0o600
      with
      | fd ->
          created := Some (path, fd);
          let channel = Unix.out_channel_of_descr fd in
          output := Some channel;
          (path, channel)
      | exception Unix.Unix_error (Unix.EEXIST, _, _) -> create (suffix + 1)
    in
    let path, channel = with_sigint_blocked (fun () -> create 1) in
    output_string channel json;
    flush channel;
    close_out channel;
    report path;
    Ok ()
  with
  | Sys.Break -> (
      match cleanup () with
      | Ok () -> raise Sys.Break
      | Error reason -> Error ("中断後のファイル削除に失敗しました: " ^ reason))
  | Unix.Unix_error (error, operation, path) ->
      fail (unix_error error operation path)
  | Sys_error reason -> fail reason
