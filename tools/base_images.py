#!/usr/bin/env python3
"""Helpers for the shared base images in bases/<name>/ and for models that build FROM them.

  base_images.py changed <base> <head>       JSON list of bases whose Dockerfile, VERSION or check.sh changed
  base_images.py only <name>                 JSON list with exactly this base (manual runs)
  base_images.py check-bump <base> <head> <json list of base names>
                                             fails if a base changed without a higher VERSION
  base_images.py check-pins <rev> <json list of model ids>
                                             fails if a model's Dockerfile builds FROM a shared base without
                                             an @sha256 digest

A base folder holds Dockerfile, VERSION (a positive integer, same rule as a model's version, see
build_version.py), check.sh (runs inside the built image) and optionally example-*.Dockerfile, which are
documentation and never trigger a build. The base is published as <registry>/<base project>/<name>:<VERSION>.

Models must name a base by digest. A tag can be moved, so the same Dockerfile could build a different
model later; the digest is printed in the job summary when the base is pushed. Only tree objects and the
files named above are read, so it works in a blobless clone.
"""
import json
import os
import re
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from build_version import version_problem

NAME_PATTERN = re.compile(r"^[a-z0-9][a-z0-9._-]*$")
RELEVANT_FILES = {"Dockerfile", "VERSION", "check.sh"}
BASE_PATH_MARKER = "/base/"          # <registry>/<project>/base/<name>... see BASE_REF
FROM_PATTERN = re.compile(r"^\s*FROM\s+(?:--platform=\S+\s+)?(\S+)", re.IGNORECASE | re.MULTILINE)
DIGEST_PATTERN = re.compile(r"@sha256:[0-9a-f]{64}$")


def git(*args):
    return subprocess.run(["git", *args], capture_output=True, text=True)


def changed_bases(base, head):
    diff = git("diff", "--name-only", base, head, "--", "bases")
    if diff.returncode != 0:
        sys.exit(f"git diff failed: {diff.stderr.strip()}")
    names = set()
    for path in diff.stdout.splitlines():
        parts = path.split("/")
        if len(parts) == 3 and parts[0] == "bases" and parts[2] in RELEVANT_FILES:
            names.add(parts[1])
    return sorted(n for n in names if NAME_PATTERN.match(n) and git("cat-file", "-e", f"{head}:bases/{n}").returncode == 0)


def version_at(rev, name):
    shown = git("show", f"{rev}:bases/{name}/VERSION")
    return shown.stdout.strip() if shown.returncode == 0 else None


def bump_problems(base, head, names):
    problems = []
    for name in names:
        new = version_at(head, name)
        if new is None:
            problems.append(f"{name}: bases/{name}/VERSION is missing")
            continue
        problem = version_problem(new)
        if problem:
            problems.append(f"{name}: {problem}")
            continue
        old = version_at(base, name)
        if old is None:
            continue  # new base
        if new == old:
            problems.append(f"{name}: files changed but VERSION is still {old}; increase bases/{name}/VERSION")
        elif int(new) < int(old):
            problems.append(f"{name}: VERSION went down ({old} -> {new})")
    return problems


def unpinned_base_refs(dockerfile_text, base_project):
    """FROM lines that point into the base project of the registry but carry no @sha256 digest."""
    bad = []
    for ref in FROM_PATTERN.findall(dockerfile_text):
        if f"/{base_project}/" in ref and not DIGEST_PATTERN.search(ref):
            bad.append(ref)
    return bad


def pin_problems(rev, ids, base_project):
    problems = []
    for model_id in ids:
        shown = git("show", f"{rev}:models/{model_id}/Dockerfile")
        if shown.returncode != 0:
            continue
        for ref in unpinned_base_refs(shown.stdout, base_project):
            problems.append(f"{model_id}: FROM {ref} must name the base by digest (@sha256:...); "
                            "the digest is in the job summary of the base build")
    return problems


def main():
    args = sys.argv[1:]
    if args[:1] == ["changed"] and len(args) == 3:
        print(json.dumps(changed_bases(args[1], args[2])))
    elif args[:1] == ["only"] and len(args) == 2:
        if not NAME_PATTERN.match(args[1]) or not os.path.isdir(f"bases/{args[1]}"):
            sys.exit(f"no base named '{args[1]}' under bases/")
        print(json.dumps([args[1]]))
    elif args[:1] == ["check-bump"] and len(args) == 4:
        problems = bump_problems(args[1], args[2], json.loads(args[3]))
        for problem in problems:
            print(f"::error::{problem}")
        sys.exit(1 if problems else 0)
    elif args[:1] == ["check-pins"] and len(args) == 3:
        problems = pin_problems(args[1], json.loads(args[2]), os.environ.get("HARBOR_BASE_PROJECT", "fskx-base"))
        for problem in problems:
            print(f"::error::{problem}")
        sys.exit(1 if problems else 0)
    else:
        sys.exit(__doc__)


if __name__ == "__main__":
    main()
