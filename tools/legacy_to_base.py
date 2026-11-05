#!/usr/bin/env python3
"""Convert the Dockerfile a legacy R 3.6.3 model was built with into a Dockerfile on the shared base image.

  legacy_to_base.py convert --history FILE --model-id ID [--models-dir DIR] [--cpu N] [--ram N]
  legacy_to_base.py convert --dockerfile FILE --model-id ID [--models-dir DIR]
  legacy_to_base.py groups  --history FILE

Two modes, chosen by the FROM line of the legacy Dockerfile: rocker/tidyverse:3.6.3-ubuntu18.04 (R 3.6.3 mode, below) and
rocker/r2u:latest, rocker/r2u:24.04 or rocker/tidyverse:4.5 (R 4.5 mode, last paragraph of this text).

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

Environment of the model (--fskx FILE, optional): the packages.json inside the FSKX file declares package versions. Scientific
models must keep their environment, so every declared version that differs from what the base has installed
(bases/r-3.6.3/packages.tsv, override with --installed) gets an install_version line, and the check step fails the build if an
installed version differs from the declared one. Cases the converter cannot decide are written as "# CHECK:" comments: a
declared version that contradicts a version pinned in the legacy lines, and a declared version the build cannot meet (list
it with --allow PACKAGE after you have looked at it; it is then left out of the check and named in a comment).
Two more switches for those cases: --drop PACKAGE removes the legacy line(s) that install only that package (for a version the
base cannot build at all, e.g. MASS 7.3-57 on R 3.6.3; the base's own version stays), and --repin PACKAGE installs the declared
version again at the end (for a package that a later install in the legacy lines upgraded, e.g. gtable).

R 4.5 mode (needs --fskx): the legacy images of these models (r2u apt packages, the floating tidyverse:4.5) cannot be rebuilt
line by line, they never pinned anything. The Dockerfile is generated from what is known about the model instead: FROM the lean
bases/r-4.5 (digest from CURATOR_GUIDE.md), CRAN pointed to the Posit Package Manager snapshot of the day the legacy image was
built (the last evidence of a working environment; --snapshot YYYY-MM-DD overrides), then the packages: install_version for
every version declared in packages.json (and for install_version lines of the legacy Dockerfile), install.packages in the
snapshot's version for all other names. The names come from the union of packages.json, the library()/require()/pkg:: calls of
the model's R scripts, the legacy r-cran-* apt names and the legacy install.packages lines. The build fails if an installed
version differs from a declared one, if a named package is missing, or if a package does not load (a missing system library).
Not carried over, with a "# CHECK:" note: other legacy lines (Makevars, system -dev libraries). Refused: Stan and JAGS models (their own
base). OpenBUGS models (R2OpenBUGS in the scripts or packages.json) go on the bugs variant of the base (bases/r-4.5-bugs, and bases/r-3.6.3-bugs
in the R 3.6.3 mode), whose digest comes from CURATOR_GUIDE.md or --bugs-from; the legacy lines that installed JAGS and compiled OpenBUGS are
dropped. Switches: --drop PACKAGE leaves a package out (a false hit of the script scan),
--allow PACKAGE installs a declared package without its version (not enforced), --repin PACKAGE installs the declared version
again at the end, --apt PACKAGE adds a system package (a package that compiles against a library, e.g. tkrplot needs tk-dev).
Every version is installed with upgrade = 'never', so a later pin cannot upgrade an earlier one.
"""
import argparse
import json
import os
import re
import sys
import zipfile
from email.utils import parsedate_to_datetime

HERE = os.path.dirname(os.path.abspath(__file__))
GUIDE = os.path.join(os.path.dirname(HERE), "CURATOR_GUIDE.md")

LEGACY_FROM = "rocker/tidyverse:3.6.3-ubuntu18.04"
DEFAULT_SNAPSHOT = "2024-11-01"
BASE_FROM_PATTERN = re.compile(r"FROM\s+(\S+/fskx-base/r-3\.6\.3@sha256:[0-9a-f]{64})")
BASE45_FROM_PATTERN = re.compile(r"FROM\s+(\S+/fskx-base/r-4\.5@sha256:[0-9a-f]{64})")
BUGS36_FROM_PATTERN = re.compile(r"FROM\s+(\S+/fskx-base/r-3\.6\.3-bugs@sha256:[0-9a-f]{64})")
BUGS45_FROM_PATTERN = re.compile(r"FROM\s+(\S+/fskx-base/r-4\.5-bugs@sha256:[0-9a-f]{64})")
LEGACY_FROM_45 = re.compile(r"FROM (rocker/r2u:(latest|24\.04)|rocker/tidyverse:4\.5)")
SNAPSHOT_45 = "https://p3m.dev/cran/__linux__/noble/{}"
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


PIN = re.compile(r"""install_version\(\s*['"]([^'"]+)['"]\s*,\s*version\s*=\s*['"]([^'"]+)['"]""")
# R's own packages follow the R version, not an install step; "base" in packages.json is R itself.
R_OWN = {"base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods", "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils"}
DEFAULT_INSTALLED = os.path.join(os.path.dirname(HERE), "bases", "r-3.6.3", "packages.tsv")
DEFAULT_INSTALLED_45 = os.path.join(os.path.dirname(HERE), "bases", "r-4.5", "packages.tsv")


class Unsupported(Exception):
    pass


def read_declared(fskx_path):
    """{package: version} of the packages.json in an FSKX file, only entries with a version, without R's own packages."""
    with zipfile.ZipFile(fskx_path) as z:
        if "packages.json" not in z.namelist():
            return {}
        listed = json.loads(z.read("packages.json")).get("PackageList") or []
    return {x["Package"]: x["Version"] for x in listed
            if isinstance(x, dict) and x.get("Package") and x.get("Version") and x["Package"] not in R_OWN}


def read_installed(path):
    with open(path, encoding="utf-8") as f:
        return dict(line.rstrip("\n").split("\t", 1) for line in f if "\t" in line)


def drop_lines(own, drop):
    """The legacy lines without those that install only a package named in drop."""
    return [raw for raw in own if not (drop and {name for name, _ in PIN.findall(raw)} and {name for name, _ in PIN.findall(raw)} <= set(drop))]


def pin_lines(own, declared, installed, allow, repin=()):
    """(install lines, notes, packages to verify) that bring the environment to the declared versions."""
    pinned = {}
    for raw in own:
        pinned.update(PIN.findall(raw))
    lines, notes, verify = [], [], {}
    for name, want in sorted(declared.items()):
        if name in allow:
            notes.append(f"packages.json declares {name} {want}, not enforced ({name} accepted by --allow)")
        elif name in pinned:
            if pinned[name] != want:
                notes.append(f"packages.json declares {name} {want} but the legacy lines install {pinned[name]}: decide which one is right")
            verify[name] = want
        else:
            verify[name] = want
            if installed.get(name) != want:
                lines.append(f"""RUN R -e "require(remotes)" -e "install_version('{name}', version = '{want}')\"""")
    for name in repin:
        if name not in declared:
            raise Unsupported(f"--repin {name}: packages.json declares no version of it")
        lines.append(f"""RUN R -e "require(remotes)" -e "install_version('{name}', version = '{declared[name]}', upgrade = 'never')\"""")
        notes.append(f"{name} {declared[name]} is installed again at the end: a later legacy line replaces it")
    return lines, notes, verify


def instructions(text):
    """Logical instructions as (raw text with its original line breaks, text with whitespace collapsed)."""
    out, current = [], []
    for line in text.splitlines():
        stripped = line.strip()
        if not current and (not stripped or stripped.startswith("#")):
            continue
        if current and current[-1].rstrip().endswith("\\") and (not stripped or stripped.startswith("#")):
            continue  # Docker drops comment and empty lines inside a continued instruction
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


# OpenBUGS: every legacy image apt-installed JAGS and 32-bit development libraries and compiled OpenBUGS 3.2.3 from the webbugs tarball.
# The bugs bases (bases/r-3.6.3-bugs, bases/r-4.5-bugs) compile it once; the model keeps only R2OpenBUGS (and its other packages).
BUGS_BUILD = re.compile(r"webbugs\.psychstat\.org|OpenBUGS-3\.2\.3")
BUGS_APT = re.compile(r"\bapt(-get)? (-\S+ )*install\b.*(\bjags\b|libc6-dev-i386|gcc-multilib)")
JAGS_PACKAGES = {"rjags", "R2jags", "runjags", "jagsUI"}
STAN_PACKAGES = {"rstan", "rstanarm", "brms", "cmdstanr", "StanHeaders"}
BUGS_PACKAGES = {"R2OpenBUGS", "BRugs", "R2WinBUGS"}


def remove_bugs(own):
    """(own without the OpenBUGS build and the apt line that installs JAGS / 32-bit libraries, whether an OpenBUGS build was there)."""
    flats = [" ".join(raw.replace("\\\n", " ").split()) for raw in own]
    found = any(BUGS_BUILD.search(f) for f in flats)
    if not found:
        return own, False
    return [raw for raw, f in zip(own, flats) if not BUGS_BUILD.search(f) and not BUGS_APT.search(f)], True


def bayes_kinds(fskx_path, declared, installed_by_legacy=()):
    """Which Bayesian tools a model really uses (its scripts and packages.json, not its legacy Dockerfile, which installed all of them)."""
    names = set(declared) | set(installed_by_legacy)
    stan_files = False
    if fskx_path:
        names |= scan_scripts(fskx_path)
        with zipfile.ZipFile(fskx_path) as z:
            stan_files = any(n.lower().endswith(".stan") for n in z.namelist())
    kinds = set()
    lower = {n.lower() for n in names}  # the legacy apt names (r-cran-rstan) are lower case
    if lower & {n.lower() for n in JAGS_PACKAGES}:
        kinds.add("jags")
    if (lower & {n.lower() for n in STAN_PACKAGES}) or stan_files:
        kinds.add("stan")
    if lower & {n.lower() for n in BUGS_PACKAGES}:
        kinds.add("openbugs")
    return kinds


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
            found.append("an unpinned install.packages line: on the R 3.6.3 base it resolves from the base's dated CRAN snapshot (2020-04-24), so it is reproducible; pin it if the model needs another version (guide step 5, rule 1)")
        if re.search(r"install_github|git clone|wget |curl ", flat):
            found.append("downloads from outside CRAN/apt: check that the source is still there")
    return sorted(set(found))

# ---- R 4.5 mode ------------------------------------------------------------------------------------------------------
BAYES_PACKAGES = {"rstan", "rstanarm", "brms", "cmdstanr", "StanHeaders", "rjags", "R2jags", "runjags", "jagsUI", "R2OpenBUGS", "BRugs",
                  "R2WinBUGS"}
BAYES_WORDS = re.compile(r"openbugs|\bjags\b|\bstan\b|rstan|cmdstan", re.I)
HARNESS_LINES = re.compile(r"miniconda|\bconda\b|\.Rprofile|environment\.yml|^(ADD|COPY) |^ENTRYPOINT\b|^FROM\b")
APT_IGNORED = {"ca-certificates", "curl", "cmake", "bash", "wget", "unzip"}
LOADS = re.compile(r"""\b(?:library|require|requireNamespace|loadNamespace)\(\s*["']?([A-Za-z][A-Za-z0-9.]*)""")
QUALIFIED = re.compile(r"""\b([A-Za-z][A-Za-z0-9.]*):::?[A-Za-z._]""")
P_LOAD = re.compile(r"\bp_load\(([^)]*)\)")
INSTALL_PACKAGES = re.compile(r"""install\.packages\(\s*(c\([^)]*\)|["'][^"']+["'])""")
NOT_PACKAGES = {"TRUE", "FALSE", "T", "F", "NULL", "NA"}


def same_version(a, b):
    """R treats '-' and '.' alike in versions: packages.json says 1.1.1.1 where CRAN's own spelling is 1.1-1.1."""
    return re.split(r"[.-]", a or "") == re.split(r"[.-]", b or "")


def uncommented(text):
    return "\n".join(line.split("#", 1)[0] if not line.lstrip().startswith("#") else "" for line in text.splitlines())


def scan_scripts(fskx_path):
    """Names of the packages the R scripts of an FSKX file load: library()/require(), pkg::fun, pacman::p_load(...)."""
    names = set()
    with zipfile.ZipFile(fskx_path) as z:
        for n in z.namelist():
            if n.lower().endswith((".r", ".rmd")):
                code = uncommented(z.read(n).decode("utf-8", "replace"))
                names.update(LOADS.findall(code))
                names.update(QUALIFIED.findall(code))
                for args in P_LOAD.findall(code):
                    for token in args.split(","):
                        token = token.strip().strip("\"'")
                        if re.fullmatch(r"[A-Za-z][A-Za-z0-9.]*", token) and token not in NOT_PACKAGES:
                            names.add(token)
    return names - R_OWN


def read_language(fskx_path):
    with zipfile.ZipFile(fskx_path) as z:
        if "packages.json" not in z.namelist():
            return ""
        return str(json.loads(z.read("packages.json")).get("Language") or "")


def legacy_45(text):
    """(apt r-cran names, install.packages names, install_version pins, other apt packages, lines not carried over)."""
    apt_r, cran, pins, apt_other, skipped = [], [], {}, [], []
    for raw, _ in instructions(text):
        flat = " ".join("\n".join(l for l in raw.splitlines() if not l.lstrip().startswith("#")).replace("\\\n", " ").split())
        if not flat or HARNESS_LINES.search(flat):
            continue
        handled = False
        if re.search(r"\bapt(-get)? (-\S+ )*install\b", flat):
            handled = True
            for part in re.split(r"&&", flat):
                if re.search(r"\bapt(-get)? (-\S+ )*install\b", part):
                    for token in part.split("install", 1)[1].split():
                        if token.startswith("-") or token == "\\":
                            continue
                        if token.startswith("r-cran-"):
                            apt_r.append(token[len("r-cran-"):])
                        elif token not in APT_IGNORED:
                            apt_other.append(token)
        if "install.packages(" in flat or "install_version(" in flat:
            handled = True
            for group in INSTALL_PACKAGES.findall(flat):
                cran += re.findall(r"""["']([^"']+)["']""", group)
            pins.update(PIN.findall(flat))
        if not handled:
            skipped.append(flat[:110])
    return apt_r, cran, pins, apt_other, skipped


def convert45(text, base_from, built, fskx_path, installed, snapshot=None, allow=(), drop=(), repin=(), source="", apt=(), bugs_from=None):
    items = instructions(text)
    if not items or not LEGACY_FROM_45.fullmatch(items[0][1]):
        raise Unsupported("does not start with FROM rocker/r2u:latest, rocker/r2u:24.04 or rocker/tidyverse:4.5")
    if not fskx_path:
        raise Unsupported("the R 4.5 mode needs --fskx: the declared versions and the packages the scripts load are read from it")
    date = snapshot or built
    if not date or date == "none":
        raise Unsupported("no build date of the legacy image: pass --snapshot YYYY-MM-DD")
    apt_r, cran, legacy_pins, apt_other, skipped = legacy_45(text)
    declared = read_declared(fskx_path)
    used = scan_scripts(fskx_path)
    kinds = bayes_kinds(fskx_path, declared, set(apt_r) | set(cran) | set(legacy_pins))
    if kinds - {"openbugs"}:
        raise Unsupported(f"Bayesian tooling ({', '.join(sorted(kinds - {'openbugs'}))}): needs its own base, write it by hand")
    bugs = "openbugs" in kinds
    if bugs:
        if not bugs_from:
            raise Unsupported("uses OpenBUGS: needs the r-4.5-bugs base; its digest is not in CURATOR_GUIDE.md yet, pass --bugs-from")
        base_from = bugs_from
    notes_ = []
    if bugs:
        apt_other = [a for a in apt_other if a not in ("jags", "gcc-multilib", "libc6-dev-i386")]
        if "jags" in text:
            notes_.append("the legacy image installed JAGS, the model does not use it (no rjags/R2jags in its scripts or packages.json): not installed")
        skipped = [l for l in skipped if "webbugs" not in l and "OpenBUGS-3.2.3" not in l]

    # spelling: packages.json, pins and install.packages lines know the CRAN case; the apt names are lower case
    spelling = {}
    for source_names in (list(declared), list(legacy_pins), [c for c in cran], sorted(used)):
        for n in source_names:
            spelling.setdefault(n.lower(), n)
    for n in installed:
        spelling.setdefault(n.lower(), n)
    names = set(declared) | set(legacy_pins) | set(cran) | used | {spelling.get(a.lower(), a) for a in apt_r}
    names = {n for n in names if n not in R_OWN and n not in drop and n not in BAYES_PACKAGES - ({"R2OpenBUGS"} if bugs else set())}
    guessed = sorted({a for a in apt_r if a.lower() not in spelling and a in names})
    if guessed:
        notes_.append("the CRAN spelling of " + ", ".join(guessed) + " is a guess from the apt name r-cran-<lower case>; the build fails if it is wrong")

    pins = {n: v for n, v in legacy_pins.items() if n in names}
    for n, want in declared.items():
        if n not in names or n in allow:
            continue
        if n in pins and pins[n] != want:
            notes_.append(f"packages.json declares {n} {want} but the legacy lines install {pins[n]}: the declared version is used")
        pins[n] = want
    for n in sorted(set(allow)):
        if n in declared:
            notes_.append(f"packages.json declares {n} {declared[n]}, not enforced ({n} accepted by --allow)")
    for n in sorted(set(drop) & set(declared)):
        notes_.append(f"packages.json declares {n} {declared[n]}, left out (--drop {n})")
    for n in repin:
        if n not in declared:
            raise Unsupported(f"--repin {n}: packages.json declares no version of it")
        notes_.append(f"{n} {declared[n]} is installed again at the end")
    if apt:
        notes_.append("system packages added by --apt: " + ", ".join(apt))

    language = read_language(fskx_path)
    m = re.search(r"\bR\s+(\d+)\.(\d+)", language)
    if m and (int(m.group(1)), int(m.group(2))) != (4, 5):
        notes_.append(f"packages.json names the language '{language}', the base is R 4.5.1: decide whether that is acceptable")
    if apt_other:
        notes_.append("system packages of the legacy image are not installed (" + ", ".join(sorted(set(apt_other))[:8])
                      + ("..." if len(set(apt_other)) > 8 else "") + "): the Posit binaries only need the runtime libraries of the base; "
                      "the load check at the end fails the build if one is missing")
    for line in skipped:
        notes_.append(f"legacy line not carried over: {line}" + ("..." if len(line) >= 110 else ""))

    pin_lines_ = [f"""R -e "require(remotes)" -e "install_version('{n}', version = '{v}', upgrade = 'never')\"""" for n, v in sorted(pins.items())
                  if not same_version(installed.get(n), v)]
    plain = sorted(n for n in names if n not in pins and n not in installed)
    lines = [f"# Converted from the legacy Dockerfile{source} by tools/legacy_to_base.py (R 4.5 mode); build and smoke-test it, then adjust by hand.",
             "# On the shared base image bases/" + ("r-4.5-bugs (= bases/r-4.5 + OpenBUGS 3.2.3 compiled once; r-4.5 is" if bugs else "r-4.5 (") + " lean rocker/r-ver 4.5.1 + curl, cmake, runtime libraries, Miniconda, the harness",
             "# conda environment, remotes, jsonlite); the base is named by digest. The environment is reproduced from a date: CRAN is the",
             f"# Posit Package Manager snapshot of {date}, the versions of packages.json are installed from it and checked, a package the",
             "# model only names comes in the version of that day."]
    lines += [f"# CHECK: {n}" for n in notes_]
    lines += [f"FROM {base_from}", "", f"""RUN echo "options(repos = c(CRAN = '{SNAPSHOT_45.format(date)}'))" > /.Rprofile""", ""]
    if apt:
        lines.append("RUN apt-get update && apt-get install -y --no-install-recommends " + " ".join(apt) + " && rm -rf /var/lib/apt/lists/*")
    if pin_lines_:
        lines.append("RUN " + " \\\n && ".join(pin_lines_))
    if plain:
        lines.append("RUN R -e \"install.packages(c(" + ", ".join(f"'{n}'" for n in plain) + "))\"")
    for n in repin:
        lines.append(f"""RUN R -e "require(remotes)" -e "install_version('{n}', version = '{declared[n]}', upgrade = 'never')\"""")
    check = sorted(n for n in names if n not in installed or n in pins)
    if check:
        quoted = ", ".join(f"'{n}'" for n in check)
        lines.append(f"""RUN R -e "missing <- setdiff(c({quoted}), rownames(installed.packages())); if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))\"""")
    verify = {n: v for n, v in pins.items() if n in names}
    if verify:
        wanted = ", ".join(f"{k} = '{v}'" for k, v in sorted(verify.items()))
        lines.append("# The environment declared in the model's packages.json (and the legacy pins): the build fails if an installed version differs.")
        lines.append(f"""RUN R -e "want <- c({wanted}); ip <- installed.packages(); got <- unname(ip[match(names(want), rownames(ip)), 'Version']); ok <- !is.na(got) & vapply(seq_along(want), function(i) !is.na(got[i]) && numeric_version(got[i]) == numeric_version(want[i]), logical(1)); got[is.na(got)] <- 'missing'; names(got) <- names(want); bad <- names(want)[!ok]; if (length(bad) > 0) stop(paste('versions differ from packages.json:', paste(bad, got[bad], 'wanted', want[bad], collapse = '; ')))\"""")
    if check:
        lines.append("# Every named package must load (a missing system library shows up here, not when the model runs).")
        lines.append(f"""RUN R -e "invisible(lapply(c({quoted}), library, character.only = TRUE))\"""")
    lines += ["", "ADD . /containerscripts", "ADD fskx /fskx", "ENTRYPOINT . ./containerscripts/entrypoint.sh", ""]
    return "\n".join(lines)


def convert(text, base_from, snapshot=None, source="", declared=None, installed=None, allow=(), drop=(), repin=(), built=None, fskx=None, apt=(), bugs_from=None):
    items = instructions(text)
    if items and LEGACY_FROM_45.fullmatch(items[0][1]):
        return convert45(text, base_from, built, fskx, installed or {}, snapshot, allow, drop, repin, source, apt, bugs_from)
    own, bugs = remove_bugs(split(text))
    if bugs:
        kinds = bayes_kinds(fskx, declared or {})
        if kinds - {"openbugs"}:
            raise Unsupported(f"Bayesian tooling ({', '.join(sorted(kinds - {'openbugs'}))}): needs its own base, write it by hand")
        if not bugs_from:
            raise Unsupported("uses OpenBUGS: needs the r-3.6.3-bugs base; its digest is not in CURATOR_GUIDE.md yet, pass --bugs-from")
        base_from = bugs_from
    own = drop_lines(own, drop)
    pins, pin_notes, verify = pin_lines(own, declared or {}, installed or {}, set(allow), repin)
    pin_notes += [f"the legacy lines installing {n} are left out: the base's version stays" for n in drop]
    own = own + pins
    date = snapshot_for(own, snapshot)
    lines = [f"# Converted from the legacy Dockerfile{source} by tools/legacy_to_base.py; build and smoke-test it, then adjust by hand.",
             "# On the shared base image bases/" + ("r-3.6.3-bugs (= bases/r-3.6.3 + OpenBUGS 3.2.3 compiled once; r-3.6.3 is" if bugs else "r-3.6.3 (")
             + " rocker/tidyverse 3.6.3-ubuntu18.04 + curl, cmake, Miniconda, the harness",
             "# conda environment, remotes, jsonlite); the base is named by digest."]
    if bugs:
        pin_notes = ["the legacy lines that apt-installed JAGS and 32-bit libraries and compiled OpenBUGS 3.2.3 are replaced by the bugs base; "
                     "the model does not use JAGS (checked in its scripts)"] + pin_notes
    lines += [f"# CHECK: {n}" for n in notes(own) + pin_notes]
    lines += [f"FROM {base_from}", ""]
    if date:
        lines += [f"""# Dated CRAN snapshot: ggplot2 >= 3.4 does not install from today's CRAN on R 3.6.3 (see bases/r-3.6.3/example-ggplot2-group.Dockerfile).""",
                  f"""RUN echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/{date}'))" > /.Rprofile""", ""]
    lines += own
    names = packages(own)
    if names:
        quoted = ", ".join(f"'{n}'" for n in names)
        lines.append(f"""RUN R -e "missing <- setdiff(c({quoted}), rownames(installed.packages())); if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))\"""")
    if verify:
        wanted = ", ".join(f"{k} = '{v}'" for k, v in sorted(verify.items()))
        lines.append("# The environment declared in the model's packages.json: the build fails if an installed version differs.")
        lines.append(f"""RUN R -e "want <- c({wanted}); ip <- installed.packages(); got <- unname(ip[match(names(want), rownames(ip)), 'Version']); got[is.na(got)] <- 'missing'; names(got) <- names(want); bad <- names(want)[got != want]; if (length(bad) > 0) stop(paste('versions differ from packages.json:', paste(bad, got[bad], 'wanted', want[bad], collapse = '; ')))\"""")
    if own:
        lines.append("")
    lines += ["ADD . /containerscripts", "ADD fskx /fskx", "ENTRYPOINT . ./containerscripts/entrypoint.sh", ""]
    return "\n".join(lines)


def default_base_from(pattern=BASE_FROM_PATTERN, name="r-3.6.3"):
    with open(GUIDE, encoding="utf-8") as f:
        m = pattern.search(f.read())
    if not m:
        sys.exit(f"no {name} base digest found in {GUIDE}; pass --base-from")
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
    built = None
    if a.dockerfile:
        with open(a.dockerfile, encoding="utf-8") as f:
            text, source = f.read(), ""
    else:
        builds = latest_builds(a.history)
        if a.model_id not in builds:
            sys.exit(f"no completed build of {a.model_id} in {a.history}")
        b = builds[a.model_id]
        built = parsedate_to_datetime(b["created_at"]).date()
        text, source = b["docker_file"], f" built {built}"
    items = instructions(text)
    mode45 = bool(items) and bool(LEGACY_FROM_45.fullmatch(items[0][1]))
    if a.base_from:
        base_from = a.base_from
    elif mode45:
        base_from = default_base_from(BASE45_FROM_PATTERN, "r-4.5")
    else:
        base_from = default_base_from()
    bugs_from = a.bugs_from
    if not bugs_from:
        try:
            bugs_from = default_base_from(BUGS45_FROM_PATTERN if mode45 else BUGS36_FROM_PATTERN, "bugs")
        except SystemExit:
            bugs_from = None
    declared = read_declared(a.fskx) if a.fskx else None
    installed = read_installed(a.installed or (DEFAULT_INSTALLED_45 if mode45 else DEFAULT_INSTALLED)) if (a.fskx or mode45) else None
    try:
        result = convert(text, base_from, a.snapshot, source, declared, installed, a.allow or [], a.drop or [], a.repin or [],
                         built=str(built) if built else None, fskx=a.fskx, apt=a.apt or [], bugs_from=bugs_from)
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
    c.add_argument("--bugs-from", help="complete image reference of the bugs base (r-3.6.3-bugs or r-4.5-bugs) for OpenBUGS models (default: the line of CURATOR_GUIDE.md)")
    c.add_argument("--base-from", help="complete image reference with digest (default: the r-3.6.3 or r-4.5 line of CURATOR_GUIDE.md)")
    c.add_argument("--snapshot", help="YYYY-MM-DD of the Posit CRAN snapshot, or none (default: R 3.6.3 mode decided by the packages, R 4.5 mode the build date of the legacy image)")
    c.add_argument("--fskx", help="the model's FSKX file: enforce the package versions of its packages.json")
    c.add_argument("--installed", help="package versions of the base, tab separated (default: bases/r-3.6.3/packages.tsv or bases/r-4.5/packages.tsv)")
    c.add_argument("--allow", action="append", help="a declared package that cannot be installed at its version (repeatable)")
    c.add_argument("--drop", action="append", help="leave out the legacy lines that install only this package (repeatable)")
    c.add_argument("--repin", action="append", help="install the declared version of this package again at the end (repeatable)")
    c.add_argument("--apt", action="append", help="R 4.5 mode: a system package the model's R packages need (repeatable), e.g. tk-dev for tkrplot")
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
