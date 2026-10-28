#!/usr/bin/env python3
"""Prints the model ids to build as a JSON list.

  changed_models.py <base> <head>     models/<id>/ folders that differ between two commits
  changed_models.py --only <id>       exactly one model (manual runs)

Folders deleted in <head> are skipped. Only tree objects are needed, so it works in a
blobless clone.
"""
import argparse
import json
import re
import subprocess
import sys

ID_PATTERN = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]*$")


def git(*args):
    return subprocess.run(["git", *args], capture_output=True, text=True)


def changed_models(base, head):
    diff = git("diff", "--name-only", base, head, "--", "models")
    if diff.returncode != 0:
        sys.exit(f"git diff failed: {diff.stderr.strip()}")
    ids = set()
    for path in diff.stdout.splitlines():
        parts = path.split("/")
        if len(parts) >= 3 and parts[0] == "models":
            ids.add(parts[1])
    return sorted(i for i in ids if ID_PATTERN.match(i) and git("cat-file", "-e", f"{head}:models/{i}").returncode == 0)


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("base", nargs="?")
    parser.add_argument("head", nargs="?")
    parser.add_argument("--only")
    args = parser.parse_args()
    if args.only:
        if not ID_PATTERN.match(args.only):
            sys.exit(f"invalid model id: {args.only}")
        ids = [args.only]
    elif args.base and args.head:
        ids = changed_models(args.base, args.head)
    else:
        parser.error("give <base> <head> or --only <id>")
    print(json.dumps(ids))


if __name__ == "__main__":
    main()
