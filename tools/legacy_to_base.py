#!/usr/bin/env python3
"""Convert the Dockerfile a legacy R 3.6.3 model was built with into a Dockerfile on the shared base image.

  legacy_to_base.py convert --history FILE --model-id ID [--models-dir DIR] [--cpu N] [--ram N]
  legacy_to_base.py convert --dockerfile FILE --model-id ID [--models-dir DIR]
  legacy_to_base.py groups  --history FILE

FILE for --history is a JSON export of the image build history, {"builds": [{"model_id", "status", "created_at",
"docker_file", ...}]}; the newest COMPLETED build of a model counts. Without --models-dir the Dockerfile is printed;
with it, models/<id>/Dockerfile and build.yaml (version 1) are written (an existing model folder is never touched).

Every R 3.6.3 legacy Dockerfile starts with the same setup (curl, cmake, an .Rprofile, Miniconda, the conda environment,
remotes, jsonlite) and ends with ADD/ENTRYPOINT; in between come the model's own lines. The base image holds the setup
already, so the converter drops those lines (only if they match exactly), keeps the model's own lines in their order and
puts ADD/ENTRYPOINT last, as the curator guide requires. It refuses a Dockerfile that does not start from
rocker/tidyverse:3.6.3-ubuntu18.04 or whose setup deviates (another Miniconda, WORKDIR, ENV, ...): those are written by hand.

It is a scaffold, not a verdict: build and smoke-test every result (curator guide step 7). Two things it decides:
* the CRAN date. ggplot2 >= 3.4 does not install from today's CRAN on R 3.6.3 (cpp11 needs R >= 4.0), so such a model gets
  the dated snapshot line of bases/r-3.6.3/example-ggplot2-group.Dockerfile; other models keep the repositories of the
  rocker image (like model 9731510d). Override with --snapshot YYYY-MM-DD or --snapshot none.
* a check step that fails the build if a package named in an install line is missing.
"""
import argparse
import json
import os
import re
import sys
from email.utils import parsedate_to_datetime

HERE = os.path.dirname(os.path.abspath(__file__))
GUIDE = os.path.join(os.path.dirname(HERE), "CURATOR_GUIDE.md")

LEGACY_FROM = "rocker/tidyverse:3.6.3-ubuntu18.04"
DEFAULT_SNAPSHOT = "2024-11-01"
BASE_FROM_PATTERN = re.compile(r"FROM\s+(\S+/fskx-base/r-3\.6\.3@sha256:[0-9a-f]{64})")
MINICONDA = "https://repo.anaconda.com/miniconda/Miniconda3-py38_23.5.0-3-Linux-x86_64.sh"

# Lines the base image already covers (compared after collapsing whitespace).
SETUP_LINES = {
    "RUN apt-get update && apt-get install -y curl",
    "RUN apt install cmake -y",
    'RUN echo "" > .Rprofile',
    f"RUN curl -sSL {MINICONDA} -o /miniconda.sh",
    "RUN bash /miniconda.sh -b -p $HOME/miniconda",
    'ENV PATH="/root/miniconda/bin:${PATH}"',
    "RUN conda init",
    "RUN conda env create -f /containerscripts/environment.yml",
    """RUN R -e "install.packages('remotes')\"""",
    """RUN R -e "install.packages('jsonlite')\"""",
}
REPOS_LINE = re.compile(r"""^RUN echo "options\(repos = c\(CRAN = '[^']+', RForge = '[^']+'\)\)" >> \.Rprofile$""")
CLOSING_LINES = {
    "ADD . /containerscripts": "ADD . /containerscripts",
    "ADD fskx /fskx": "ADD fskx /fskx",
    "ENTRYPOINT . ./containerscripts/entrypoint.sh": "ENTRYPOINT . ./containerscripts/entrypoint.sh",
}
# A model line that touches the harness setup is not ours to move around.
SETUP_WORDS = re.compile(r"conda|environment\.yml|\.Rprofile|containerscripts|^(ENTRYPOINT|CMD|WORKDIR|ENV|USER|ARG)\b|^FROM\b")
PACKAGE = re.compile(r"""install_version\(\s*['"]([^'"]+)['"]""")
PACKAGE_UNPINNED = re.compile(r"""install\.packages\(\s*(?:c\()?['"]([^'"]+)['"]""")
GGPLOT2 = re.compile(r"""install_version\(\s*['"]ggplot2['"]\s*,\s*version\s*=\s*['"](\d+)\.(\d+)""")


class Unsupported(Exception):
    pass


def instructions(text):
    """Logical instructions as (raw text with its original line breaks, text with whitespace collapsed)."""
    out, current = [], []
    for line in text.splitlines():
        stripped = line.strip()
        if not current and (not stripped or stripped.startswith("#")):
            continue
        current.append(line.rstrip())
        if not stripped.endswith("\\"):
            raw = "\n".join(current)
            out.append((raw, " ".join(raw.replace("\\\n", " ").split())))
            current = []
    if current:
        raw = "\n".join(current)
        out.append((raw, " ".join(raw.replace("\\\n", " ").split())))
    return out


def split(text):
    """The model's own instructions (raw), or Unsupported."""
    items = instructions(text)
    if not items or items[0][1] != f"FROM {LEGACY_FROM}":
        raise Unsupported(f"does not start with FROM {LEGACY_FROM}")
    own = []
    for raw, flat in items[1:]:
        if flat in SETUP_LINES or flat in CLOSING_LINES or REPOS_LINE.match(flat):
            continue
        if SETUP_WORDS.search(flat):
            raise Unsupported(f"touches the harness setup, write it by hand: {flat[:100]}")
        own.append(raw)
    return own


def snapshot_for(own, snapshot):
    if snapshot:
        return None if snapshot == "none" else snapshot
    for raw in own:
        m = GGPLOT2.search(raw)
        if m and (int(m.group(1)), int(m.group(2))) >= (3, 4):
            return DEFAULT_SNAPSHOT
    return None


def packages(own):
    names = []
    for raw in own:
        for n in PACKAGE.findall(raw) + PACKAGE_UNPINNED.findall(raw):
            if n not in names and not n.startswith(("http:", "https:")):
                names.append(n)
    return names


def notes(own):
    found = []
    for raw in own:
        flat = " ".join(raw.split())
        if PACKAGE_UNPINNED.search(flat):
            found.append("an unpinned install.packages line: pin it (guide step 5, rule 1)")
        if re.search(r"install_github|git clone|wget |curl ", flat):
            found.append("downloads from outside CRAN/apt: check that the source is still there")
    return sorted(set(found))


def convert(text, base_from, snapshot=None, source=""):
    own = split(text)
    date = snapshot_for(own, snapshot)
    lines = [f"# Converted from the legacy Dockerfile{source} by tools/legacy_to_base.py; build and smoke-test it, then adjust by hand.",
             "# On the shared base image bases/r-3.6.3 (rocker/tidyverse 3.6.3-ubuntu18.04 + curl, cmake, Miniconda, the harness",
             "# conda environment, remotes, jsonlite); the base is named by digest."]
    lines += [f"# CHECK: {n}" for n in notes(own)]
    lines += [f"FROM {base_from}", ""]
    if date:
        lines += [f"""# Dated CRAN snapshot: ggplot2 >= 3.4 does not install from today's CRAN on R 3.6.3 (see bases/r-3.6.3/example-ggplot2-group.Dockerfile).""",
                  f"""RUN echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/{date}'))" > /.Rprofile""", ""]
    lines += own
    names = packages(own)
    if names:
        quoted = ", ".join(f"'{n}'" for n in names)
        lines.append(f"""RUN R -e "missing <- setdiff(c({quoted}), rownames(installed.packages())); if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))\"""")
    if own:
        lines.append("")
    lines += ["ADD . /containerscripts", "ADD fskx /fskx", "ENTRYPOINT . ./containerscripts/entrypoint.sh", ""]
    return "\n".join(lines)


def default_base_from():
    with open(GUIDE, encoding="utf-8") as f:
        m = BASE_FROM_PATTERN.search(f.read())
    if not m:
        sys.exit(f"no r-3.6.3 base digest found in {GUIDE}; pass --base-from")
    return m.group(1)


def latest_builds(path):
    with open(path, encoding="utf-8") as f:
        builds = json.load(f)["builds"]
    latest = {}
    for b in builds:
        if b.get("status") != "COMPLETED" or not b.get("docker_file"):
            continue
        t = parsedate_to_datetime(b["created_at"])
        if b["model_id"] not in latest or t > parsedate_to_datetime(latest[b["model_id"]]["created_at"]):
            latest[b["model_id"]] = b
    return latest


def cmd_convert(a):
    base_from = a.base_from or default_base_from()
    if a.dockerfile:
        with open(a.dockerfile, encoding="utf-8") as f:
            text, source = f.read(), ""
    else:
        builds = latest_builds(a.history)
        if a.model_id not in builds:
            sys.exit(f"no completed build of {a.model_id} in {a.history}")
        b = builds[a.model_id]
        text, source = b["docker_file"], f" built {parsedate_to_datetime(b['created_at']).date()}"
    try:
        result = convert(text, base_from, a.snapshot, source)
    except Unsupported as e:
        sys.exit(f"{a.model_id}: {e}")
    if not a.models_dir:
        print(result, end="")
        return
    folder = os.path.join(a.models_dir, a.model_id)
    if os.path.exists(folder):
        sys.exit(f"{folder} exists, not touching it")
    os.makedirs(folder)
    with open(os.path.join(folder, "Dockerfile"), "w", encoding="utf-8") as f:
        f.write(result)
    with open(os.path.join(folder, "build.yaml"), "w", encoding="utf-8") as f:
        f.write(f"version: 1\ncpu: {a.cpu}\nram: {a.ram}\ngpu: 0\n")
    print(f"wrote {folder}")


def cmd_groups(a):
    groups, refused = {}, {}
    for model_id, b in sorted(latest_builds(a.history).items()):
        if LEGACY_FROM not in b["docker_file"]:
            continue
        try:
            own = split(b["docker_file"])
        except Unsupported as e:
            refused[model_id] = str(e)
            continue
        groups.setdefault(tuple(" ".join(r.split()) for r in own), []).append(model_id)
    for key, ids in sorted(groups.items(), key=lambda kv: -len(kv[1])):
        print(f"{len(ids):4d} models, e.g. {ids[0]}: " + (" | ".join(key) if key else "(no model-specific lines)"))
    for model_id, why in refused.items():
        print(f"   refused {model_id}: {why}")
    print(f"{sum(len(v) for v in groups.values())} convertible in {len(groups)} groups, {len(refused)} refused")


def main():
    p = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    sub = p.add_subparsers(dest="cmd", required=True)
    c = sub.add_parser("convert")
    c.add_argument("--history")
    c.add_argument("--dockerfile")
    c.add_argument("--model-id", required=True)
    c.add_argument("--models-dir")
    c.add_argument("--base-from", help="complete image reference with digest (default: the r-3.6.3 line of CURATOR_GUIDE.md)")
    c.add_argument("--snapshot", help="YYYY-MM-DD of the Posit CRAN snapshot, or none (default: decided by the packages)")
    c.add_argument("--cpu", default="1")
    c.add_argument("--ram", default="1")
    g = sub.add_parser("groups")
    g.add_argument("--history", required=True)
    a = p.parse_args()
    if a.cmd == "convert" and bool(a.history) == bool(a.dockerfile):
        p.error("give exactly one of --history and --dockerfile")
    if a.cmd == "convert" and a.snapshot and a.snapshot != "none" and not re.fullmatch(r"\d{4}-\d{2}-\d{2}", a.snapshot):
        p.error("--snapshot must be YYYY-MM-DD or none")
    {"convert": cmd_convert, "groups": cmd_groups}[a.cmd](a)


if __name__ == "__main__":
    main()
