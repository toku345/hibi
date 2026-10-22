"""PTY regressions for editing and cancellation; only synthetic temporary data."""

import errno
import fcntl
import json
import os
from pathlib import Path
import pty
import select
import signal
import struct
import subprocess
import sys

if sys.version_info < (3, 9):
    sys.exit("端末入力テストにはPython 3.9以上が必要です。")

import tempfile
import termios
import time


QUESTIONS = [
    "今日の仕事を終えて、気持ちは？",
    "今日やれたこと・頑張ったことは？",
    "今日、心が動いた場面は？",
    "明日、少し変えてみたいことは？",
]
BINARY = str(Path(sys.argv[1]).resolve())


def attributes(fd):
    settings = termios.tcgetattr(fd)
    settings[6] = [value.hex() if isinstance(value, bytes) else value
                   for value in settings[6]]
    return settings


class Terminal:
    def __init__(self, root, model="xterm-256color", **overrides):
        self.directory = root / "logs"
        self.output = bytearray()
        self.position = 0
        self.status = None
        env = dict(os.environ, HIBI_LOG_DIR=str(self.directory), TERM=model,
                   TMPDIR=str(root), **overrides)
        incoming, outgoing = os.pipe()
        self.pid, self.fd = pty.fork()
        if self.pid == 0:
            os.close(incoming)
            settings = termios.tcgetattr(0)
            settings[0] &= ~getattr(termios, "IUTF8", 0x4000)
            settings[6][termios.VERASE] = b"\x7f"
            termios.tcsetattr(0, termios.TCSANOW, settings)
            fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 80, 0, 0))
            os.write(outgoing, json.dumps(attributes(0)).encode())
            os.close(outgoing)
            os.execve(BINARY, [BINARY], env)
        os.close(outgoing)
        with os.fdopen(incoming) as snapshot:
            self.original = json.load(snapshot)

    def pump(self, timeout):
        if not select.select([self.fd], [], [], timeout)[0]:
            return False
        try:
            data = os.read(self.fd, 65536)
        except OSError as error:
            if error.errno != errno.EIO:
                raise
            return False
        self.output.extend(data)
        return bool(data)

    def expect(self, text):
        token = text.encode()
        deadline = time.monotonic() + 10
        while True:
            found = self.output.find(token, self.position)
            if found >= 0:
                self.position = found + len(token)
                return
            assert time.monotonic() < deadline, (text, bytes(self.output[-2000:]))
            self.pump(0.05)

    def question(self, index):
        self.expect(QUESTIONS[index])
        self.expect("> ")

    def send(self, data):
        os.write(self.fd, data)

    def finish(self, expected_code):
        deadline = time.monotonic() + 10
        while self.status is None:
            assert time.monotonic() < deadline, bytes(self.output[-2000:])
            self.pump(0.05)
            pid, status = os.waitpid(self.pid, os.WNOHANG)
            if pid:
                self.status = status
        while self.pump(0):
            pass
        assert os.waitstatus_to_exitcode(self.status) == expected_code, bytes(self.output)
        assert attributes(self.fd) == self.original, "terminal settings not restored"
        assert b"Fatal error" not in self.output
        return [json.loads(path.read_text(encoding="utf-8"))
                for path in sorted(self.directory.glob("*.json"))]

    def __enter__(self):
        return self

    def __exit__(self, *_):
        if self.status is None:
            try:
                os.kill(self.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            os.waitpid(self.pid, 0)
        os.close(self.fd)


def edit_case(root, data, expected, model="xterm-256color", **env):
    with Terminal(root, model=model, **env) as terminal:
        terminal.question(0)
        terminal.send(b"4\n")
        terminal.question(1)
        terminal.send(data)
        terminal.question(2)
        terminal.send(b"\n")
        terminal.question(3)
        terminal.send(b"\n")
        entries = terminal.finish(0)
        assert len(entries) == 1
        assert entries[0]["accomplishments"] == expected, entries


def run(root):
    for mode in ["pipe", "file"]:
        folder = root / f"redirect-{mode}"
        folder.mkdir()
        directory = folder / "logs"
        master, slave = pty.openpty()
        try:
            original = attributes(slave)
            os.write(master, b"4\npartial\x04\n\n\n")
            with tempfile.TemporaryFile() as redirected:
                result = subprocess.run(
                    [BINARY], stdin=slave,
                    stdout=subprocess.PIPE if mode == "pipe" else redirected,
                    stderr=subprocess.PIPE,
                    env=dict(os.environ, HIBI_LOG_DIR=str(directory)), timeout=10)
                redirected.seek(0)
                output = result.stdout if mode == "pipe" else redirected.read()
            assert result.returncode == 1, (output, result.stderr)
            assert "標準出力をパイプやファイルに変更できません".encode() in result.stderr
            assert "保存しました:".encode() not in output
            assert not directory.exists()
            assert attributes(slave) == original, "terminal settings changed"
        finally:
            os.close(master)
            os.close(slave)
        print("OK: terminal input with redirected stdout rejected:", mode)

    cases = [
        ("Ctrl-H Japanese", "日本語".encode() + b"\x08\n", "日本"),
        ("Delete Japanese", "日本語".encode() + b"\x7f\n", "日本"),
        ("Ctrl-A/E insertion", "日本".encode() + b"\x01" + "先頭".encode()
         + b"\x05" + "末尾\n".encode(), "先頭日本末尾"),
        ("Ctrl-U replacement", "日本語".encode() + b"\x15" + "変更\n".encode(), "変更"),
        ("Ctrl-U prefix only", "日本語".encode() + b"\x02\x15" + "新\n".encode(), "新語"),
        ("Ctrl-U clear answer", "日本語".encode() + b"\x15\n", None),
        ("backspace at beginning", b"\x08\x7f" + "日本\n".encode(), "日本"),
        ("Alt-Enter stays single line", b"before\x1b\rafter\n", "beforeafter"),
    ]
    for index, (name, data, expected) in enumerate(cases):
        folder = root / str(index)
        folder.mkdir()
        edit_case(folder, data, expected, model="tmux-256color" if index % 2 else "xterm-256color")
        print("OK:", name)

    for name, key, code in [("Ctrl-C", b"\x03", 130), ("Ctrl-D", b"\x04", 1),
                            ("SIGINT", None, 130)]:
        folder = root / name
        folder.mkdir()
        with Terminal(folder) as terminal:
            terminal.question(0)
            terminal.send(b"4\n")
            terminal.question(1)
            terminal.send("入力の途中".encode())
            if key is None:
                os.kill(terminal.pid, signal.SIGINT)
            else:
                terminal.send(key)
            assert terminal.finish(code) == []
            assert "保存しました:".encode() not in terminal.output
        print("OK:", name, "discards answers and restores terminal")

    folder = root / "blank"
    folder.mkdir()
    with Terminal(folder) as terminal:
        for index in range(4):
            terminal.question(index)
            terminal.send(b"\n")
        assert terminal.finish(0) == []
        assert not terminal.directory.exists()
    print("OK: all questions skipped")

    folder = root / "empty-eof"
    folder.mkdir()
    with Terminal(folder) as terminal:
        terminal.question(0)
        terminal.send(b"\x04")
        assert terminal.finish(1) == []
    print("OK: Ctrl-D on an empty answer")

    folder = root / "save-failure"
    folder.mkdir()
    blocked = folder / "logs"
    blocked.write_text("keep", encoding="utf-8")
    with Terminal(folder) as terminal:
        for index in range(4):
            terminal.question(index)
            terminal.send(b"4\n" if index == 0 else b"\n")
        assert terminal.finish(1) == []
        assert "保存しました:".encode() not in terminal.output
        assert blocked.read_text(encoding="utf-8") == "keep"
    print("OK: save failure after editing restores terminal")

    folder = root / "no-editor"
    folder.mkdir()
    marker = folder / "editor-launched"
    editor = folder / "editor"
    editor.write_text(f"#!/bin/sh\ntouch '{marker}'\n", encoding="utf-8")
    editor.chmod(0o700)
    edit_case(folder, b"before\x18\x05after\n", "beforeafter", EDITOR=str(editor))
    assert not marker.exists(), "external editor launched"
    print("OK: external editor disabled")

    for index, (data, code, count) in enumerate([
        ("4\n日本語\n\n\n", 0, 1),
        ("4\n達成\n場面\n未確定", 1, 0),
        ("\n\n\n\n", 0, 0),
        ("0\n01\n5\n\n\n\n", 0, 1),
    ]):
        directory = root / f"pipe-{index}"
        env = dict(os.environ, HIBI_LOG_DIR=str(directory))
        result = subprocess.run([BINARY], input=data, text=True, capture_output=True,
                                env=env, timeout=10)
        assert result.returncode == code, result.stderr
        files = list(directory.glob("*.json"))
        assert len(files) == count
        if index == 3:
            assert json.loads(files[0].read_text(encoding="utf-8"))["score"] == 5
            assert result.stdout.count("1〜5の整数を入力してください") == 2
    print("OK: pipe input, partial EOF, skipped answers and invalid scores")


with tempfile.TemporaryDirectory(prefix="hibi-terminal-", dir="/tmp") as temporary:
    run(Path(temporary))
print("Terminal input regressions passed")
