#!/usr/bin/env python3
"""Fails if a model that already existed was changed without raising `version` in build.yaml.

  check_version_bump.py <base> <head> <json list of model ids>

A pushed version tag is never overwritten (the build workflow refuses it), so an update
without a new version would only fail after the merge. This runs on pull requests too.
New models (no build.yaml at <base>) are fine. Needs the build.yaml blobs of both commits;
in a blobless clone git fetches them on demand.
"""
import json
import subprocess
import sys


def version_at(rev, model_id):
    shown = subprocess.run(["git", "show", f"{rev}:models/{model_id}/build.yaml"], capture_output=True, text=True)
    if shown.returncode != 0:
        return None
    for line in shown.stdout.splitlines():
        key, _, value = line.partition(":")
        if key.strip() == "version":
            return value.split("#")[0].strip()
    return None


def as_number(version):
    try:
        return int(version)
    except (TypeError, ValueError):
        return None


def check(base, head, ids):
    problems = []
    for model_id in ids:
        old = version_at(base, model_id)
        if old is None:
            continue  # new model
        new = version_at(head, model_id)
        if new is None:
            problems.append(f"{model_id}: build.yaml has no 'version'")
        elif new == old:
            problems.append(f"{model_id}: files changed but version is still {old}; increase 'version' in models/{model_id}/build.yaml")
        elif as_number(old) is not None and as_number(new) is not None and as_number(new) < as_number(old):
            problems.append(f"{model_id}: version went down ({old} -> {new})")
    return problems


def main():
    if len(sys.argv) != 4:
        sys.exit(__doc__)
    problems = check(sys.argv[1], sys.argv[2], json.loads(sys.argv[3]))
    for problem in problems:
        print(f"::error::{problem}")
    sys.exit(1 if problems else 0)


if __name__ == "__main__":
    main()
