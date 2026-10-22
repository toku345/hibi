"""公開CMIを使い、正常なクライアントとprivate recordの拒否を別々に確認する。"""

import os
from pathlib import Path
import subprocess
import sys
import tempfile


def main():
    compiler, *interfaces = sys.argv[1:]
    includes = []
    for directory in dict.fromkeys(str(Path(path).absolute().parent) for path in interfaces):
        includes.extend(["-I", directory])

    with tempfile.TemporaryDirectory(prefix="hibi-entry-boundary-") as output:
        for name, should_compile in [
            ("create", True),
            ("direct_create", False),
            ("update", False),
        ]:
            result = subprocess.run(
                [
                    compiler, "-color", "never", *includes, "-c",
                    "-o", str(Path(output) / f"{name}.cmo"),
                    str(Path("entry_boundary") / f"{name}.ml"),
                ],
                capture_output=True,
                text=True,
                env={**os.environ, "LC_ALL": "C", "OCAML_COLOR": "never"},
            )
            if should_compile:
                passed = result.returncode == 0
            else:
                # 依存不足・構文エラー・クラッシュを期待した拒否として扱わない。
                passed = (
                    result.returncode == 2
                    and "Cannot create values of the private type" in result.stderr
                )
            if not passed:
                expectation = "compile successfully" if should_compile else "reject the private record"
                raise SystemExit(
                    f"{name}: expected compiler to {expectation}, exit={result.returncode}\n"
                    f"{result.stdout}{result.stderr}"
                )
            print(f"Entry boundary: {name}: ok")


if __name__ == "__main__":
    main()
