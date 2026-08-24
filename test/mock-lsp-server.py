#!/usr/bin/env python3
"""A scripted LSP server for Flycheck's integration specs.

Speaks LSP over stdio, using only the standard library.  The scenario is
picked by the first argument:

  push    after didOpen, publish one warning, then republish the
          identical set four more times, the way a server republishes
          freely while it indexes or builds
  change  publish one warning, then a set with an error added, so the
          client has one real change to react to
  pull    advertise diagnosticProvider and answer
          textDocument/diagnostic requests, and follow each answer with
          the same diagnostics as a push: the two halves of one answer
          that Emacs 31's Eglot receives.  Also publishes once on
          didOpen, like a real server, so a client that never pulls
          (Emacs 30's Eglot) still hears about the diagnostics

Every incoming method is counted, and the counts are rewritten to the
file named by the MOCK_LSP_STATS environment variable after each
message, one "method count" pair per line.  A spec reads that file to
assert the client did not storm the server with requests.
"""

import json
import os
import sys

scenario = sys.argv[1] if len(sys.argv) > 1 else "push"
stats_file = os.environ.get("MOCK_LSP_STATS")
counts = {}

stdin = sys.stdin.buffer
stdout = sys.stdout.buffer


def read_message():
    length = None
    while True:
        line = stdin.readline()
        if not line:
            return None
        line = line.strip()
        if not line:
            break
        if line.lower().startswith(b"content-length:"):
            length = int(line.split(b":")[1])
    if length is None:
        return None
    body = stdin.read(length)
    if len(body) < length:
        # The client died mid-message; exit cleanly rather than raise
        return None
    return json.loads(body.decode("utf-8"))


def send(message):
    body = json.dumps(message).encode("utf-8")
    stdout.write(b"Content-Length: %d\r\n\r\n" % len(body))
    stdout.write(body)
    stdout.flush()


def respond(msg, result):
    send({"jsonrpc": "2.0", "id": msg["id"], "result": result})


def notify(method, params):
    send({"jsonrpc": "2.0", "method": method, "params": params})


def count(method):
    counts[method] = counts.get(method, 0) + 1
    if stats_file:
        # Written atomically so a spec reading mid-update never sees a
        # torn line, and guarded because teardown may have removed the
        # directory before the last messages are processed
        try:
            with open(stats_file + ".tmp", "w") as f:
                for name, n in sorted(counts.items()):
                    f.write("%s %d\n" % (name, n))
            os.replace(stats_file + ".tmp", stats_file)
        except OSError:
            pass


WARNING = {
    "range": {
        "start": {"line": 0, "character": 0},
        "end": {"line": 0, "character": 4},
    },
    "severity": 2,
    "code": "M1",
    "message": "mock warning",
}

ERROR = {
    "range": {
        "start": {"line": 1, "character": 0},
        "end": {"line": 1, "character": 3},
    },
    "severity": 1,
    "code": "M2",
    "message": "mock error",
}


def capabilities():
    caps = {"textDocumentSync": 1}
    if scenario == "pull":
        caps["diagnosticProvider"] = {
            "interFileDependencies": False,
            "workspaceDiagnostics": False,
        }
    elif scenario == "workspace":
        caps["diagnosticProvider"] = {
            "interFileDependencies": True,
            "workspaceDiagnostics": True,
        }
    return caps


# The "workspace" scenario is a pure pull-model server: it never
# publishes, answers a document pull with one warning, and reports on
# its workspace with that document plus a sibling file nobody opened.
# A pull naming the result id of the last report gets "unchanged".
opened = []


def document_report(uri, previous):
    if previous == "doc-1":
        return {"kind": "unchanged", "resultId": "doc-1"}
    return {"kind": "full", "resultId": "doc-1", "items": [WARNING]}


def workspace_report(previous_ids):
    known = {entry["uri"]: entry["value"] for entry in previous_ids}
    items = []
    for uri in opened:
        report = document_report(uri, known.get(uri))
        report["uri"] = uri
        report["version"] = None
        items.append(report)
        other = uri.rsplit("/", 1)[0] + "/other.mk"
        if known.get(other) == "other-1":
            items.append({"uri": other, "version": None,
                          "kind": "unchanged", "resultId": "other-1"})
        else:
            items.append({"uri": other, "version": None, "kind": "full",
                          "resultId": "other-1", "items": [ERROR]})
    if items and all(item["kind"] == "unchanged" for item in items):
        count("workspace/diagnostic/unchanged")
    return {"items": items}


def publish(uri, diags):
    notify(
        "textDocument/publishDiagnostics",
        {"uri": uri, "diagnostics": diags},
    )


def on_did_open(uri):
    if scenario == "workspace":
        opened.append(uri)
    elif scenario == "push":
        for _ in range(5):
            publish(uri, [WARNING])
    elif scenario == "change":
        publish(uri, [WARNING])
        publish(uri, [WARNING, ERROR])
    elif scenario == "pull":
        publish(uri, [WARNING])


while True:
    msg = read_message()
    if msg is None:
        break
    method = msg.get("method")
    if method:
        count(method)
    if method == "initialize":
        respond(msg, {"capabilities": capabilities()})
    elif method == "textDocument/didOpen":
        on_did_open(msg["params"]["textDocument"]["uri"])
    elif method == "textDocument/diagnostic":
        if scenario == "workspace":
            respond(msg, document_report(
                msg["params"]["textDocument"]["uri"],
                msg["params"].get("previousResultId")))
        else:
            respond(msg, {"kind": "full", "items": [WARNING]})
            publish(msg["params"]["textDocument"]["uri"], [WARNING])
    elif method == "workspace/diagnostic":
        respond(msg, workspace_report(
            msg["params"].get("previousResultIds", [])))
    elif method == "shutdown":
        respond(msg, None)
    elif method == "exit":
        break
    elif method and "id" in msg:
        # Politely decline anything else the client asks for
        respond(msg, None)
