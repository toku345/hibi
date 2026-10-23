公開CMIを使い、生成関数の呼び出しと全フィールドの読み取りができることを確認する。

  $ export LC_ALL=C OCAML_COLOR=never
  $ sh ./entry_boundary_ocamlc.sh -c -o create.cmo entry_boundary/create.ml

型の合う値でも、直接構築はprivate recordのエラーで拒否される。

  $ sh ./entry_boundary_ocamlc.sh -c -o direct_create.cmo entry_boundary/direct_create.ml 2>direct_create.err
  [2]
  $ grep -qF "Cannot create values of the private type" direct_create.err || { cat direct_create.err; false; }

レコード更新も同じ理由で拒否される。終了コードと診断を別々に検証し、
依存不足や構文エラーを期待した拒否として扱わない。

  $ sh ./entry_boundary_ocamlc.sh -c -o update.cmo entry_boundary/update.ml 2>update.err
  [2]
  $ grep -qF "Cannot create values of the private type" update.err || { cat update.err; false; }
