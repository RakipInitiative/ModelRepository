#!/usr/bin/env python3
"""Curator and CI helper for models/<model_id>/.

  prepare_model.py init <file.fskx> [--dockerfile my.Dockerfile] [--models-dir models]
                        [--cpu 1] [--ram 1] [--gpu 0]
      Creates models/<model_id>/ with Dockerfile, build.yaml and model.fskx.
      With --dockerfile the curator's hand-written Dockerfile is used as is.
      Without it, a Dockerfile is generated from the packages in the FSKX file; this
      is only a starting point and often needs manual fixes (old package versions,
      system libraries). The committed Dockerfile is the source of truth: the
      build workflow never regenerates it.

  prepare_model.py fetch models/<model_id> <out.fskx>
      For models with source.yaml (url + sha256) instead of model.fskx: downloads the
      file and verifies the checksum.

  prepare_model.py info models/<model_id> [--fskx file.fskx]
      Prints key=value lines (name, version, cpu, ram, gpu, harness_version) for the
      build workflow. Fails if the folder name differs from the model identifier.

  prepare_model.py context models/<model_id> <out_dir> [--fskx file.fskx]
      Assembles the Docker build context: the harness files at the top level
      and the unpacked model in fskx/. Build with
      docker buildx build --platform linux/amd64 -f models/<model_id>/Dockerfile <out_dir>

Needs Python >= 3.10.
"""
import argparse
import hashlib
import json
import os
import shutil
import sys
import tempfile
import urllib.request
import zipfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from FskxFile import FskxFile
from fskx_dockerfile import dockerfile_from_model

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
HARNESS_DIR = os.path.join(REPO_ROOT, "harness")


def extract_fskx(fskx_path, dest_dir):
    with zipfile.ZipFile(fskx_path, "r") as zip_ref:
        zip_ref.extractall(dest_dir)


def build_yaml(version=1, cpu=1, ram=1, gpu=0):
    return f"version: {version}\ncpu: {cpu}\nram: {ram}\ngpu: {gpu}\n"


def read_general_information(fskx_path):
    with zipfile.ZipFile(fskx_path) as z:
        with z.open("metaData.json") as f:
            return json.load(f)["generalInformation"]


def init_model(fskx_path, models_dir, cpu, ram, gpu, dockerfile_path=None):
    if dockerfile_path:
        # Hand-written Dockerfile: no generator, so no dependency scan is needed.
        with open(dockerfile_path) as f:
            dockerfile = f.read()
        info = read_general_information(fskx_path)
        model_id, model_name, language = info["identifier"], info["name"], "hand-written Dockerfile"
    else:
        with tempfile.TemporaryDirectory() as tmp:
            extract_fskx(fskx_path, tmp)
            model = FskxFile(tmp)
            dockerfile = dockerfile_from_model(model)
        model_id, model_name = model.model_id, model.model_name
        language = f"generated for {model.programming_language} {model.programming_version}"

    model_dir = os.path.join(models_dir, model_id)
    if os.path.exists(model_dir):
        sys.exit(f"{model_dir} already exists; refusing to overwrite.")
    os.makedirs(model_dir)
    with open(os.path.join(model_dir, "Dockerfile"), "w") as f:
        f.write(dockerfile)
    with open(os.path.join(model_dir, "build.yaml"), "w") as f:
        f.write(build_yaml(cpu=cpu, ram=ram, gpu=gpu))
    shutil.copy2(fskx_path, os.path.join(model_dir, "model.fskx"))
    print(f"{model_name} ({language})")
    print(f"created {model_dir}: Dockerfile, build.yaml, model.fskx")
    return model_dir


def read_simple_yaml(path):
    """Flat `key: value` files (build.yaml, source.yaml); no PyYAML needed on the runner."""
    values = {}
    with open(path) as f:
        for line in f:
            line = line.split(" #")[0].strip()
            if not line or line.startswith("#"):
                continue
            key, _, value = line.partition(":")
            values[key.strip()] = value.strip()
    return values


def read_build_yaml(model_dir):
    values = read_simple_yaml(os.path.join(model_dir, "build.yaml"))
    missing = [k for k in ("version", "cpu", "ram", "gpu") if k not in values]
    if missing:
        sys.exit(f"{model_dir}/build.yaml: missing {', '.join(missing)}")
    return values


def fetch_source(model_dir, out_path):
    source = read_simple_yaml(os.path.join(model_dir, "source.yaml"))
    url, expected = source.get("url", ""), source.get("sha256", "").lower()
    if not url.startswith("https://") or len(expected) != 64:
        sys.exit(f"{model_dir}/source.yaml needs an https url and a 64 character sha256.")
    digest = hashlib.sha256()
    with urllib.request.urlopen(url, timeout=60) as response, open(out_path, "wb") as out:
        for chunk in iter(lambda: response.read(1 << 20), b""):
            digest.update(chunk)
            out.write(chunk)
    if digest.hexdigest() != expected:
        os.remove(out_path)
        sys.exit(f"sha256 mismatch for {url}: expected {expected}, got {digest.hexdigest()}")
    print(f"downloaded {url} ({os.path.getsize(out_path)} bytes, sha256 ok)")


def resolve_fskx(model_dir, fskx_path=None):
    fskx_path = fskx_path or os.path.join(model_dir, "model.fskx")
    if not os.path.isfile(fskx_path):
        sys.exit(f"{fskx_path} not found (models using source.yaml must be fetched first).")
    info = read_general_information(fskx_path)
    folder = os.path.basename(os.path.normpath(model_dir))
    if info["identifier"] != folder:
        sys.exit(f"folder name {folder} differs from the model identifier {info['identifier']}.")
    return fskx_path, info


def print_info(model_dir, fskx_path=None):
    fskx_path, info = resolve_fskx(model_dir, fskx_path)
    build = read_build_yaml(model_dir)
    with open(os.path.join(HARNESS_DIR, "HARNESS_VERSION")) as f:
        harness_version = f.read().strip()
    name = " ".join(info["name"].split())
    for key, value in [("name", name), ("version", build["version"]), ("cpu", build["cpu"]),
                       ("ram", build["ram"]), ("gpu", build["gpu"]), ("harness_version", harness_version)]:
        print(f"{key}={value}")


def assemble_context(model_dir, out_dir, fskx_path=None):
    fskx_path, _ = resolve_fskx(model_dir, fskx_path)
    if os.path.exists(out_dir) and os.listdir(out_dir):
        sys.exit(f"{out_dir} is not empty.")
    os.makedirs(out_dir, exist_ok=True)
    shutil.copytree(HARNESS_DIR, out_dir, dirs_exist_ok=True)
    # HARNESS_VERSION is read by the workflow, not by the container.
    os.remove(os.path.join(out_dir, "HARNESS_VERSION"))
    extract_fskx(fskx_path, os.path.join(out_dir, "fskx"))
    print(f"build context ready in {out_dir}")


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest="command", required=True)

    p_init = sub.add_parser("init", help="create models/<model_id>/ from an FSKX file")
    p_init.add_argument("fskx")
    p_init.add_argument("--dockerfile", help="use this hand-written Dockerfile instead of generating one")
    p_init.add_argument("--models-dir", default=os.path.join(REPO_ROOT, "models"))
    p_init.add_argument("--cpu", type=int, default=1)
    p_init.add_argument("--ram", type=int, default=1, help="GB")
    p_init.add_argument("--gpu", type=int, default=0)

    p_fetch = sub.add_parser("fetch", help="download the FSKX file named in source.yaml")
    p_fetch.add_argument("model_dir")
    p_fetch.add_argument("out")

    p_info = sub.add_parser("info", help="print build metadata as key=value lines")
    p_info.add_argument("model_dir")
    p_info.add_argument("--fskx")

    p_ctx = sub.add_parser("context", help="assemble the Docker build context")
    p_ctx.add_argument("model_dir")
    p_ctx.add_argument("out_dir")
    p_ctx.add_argument("--fskx")

    args = parser.parse_args()
    if args.command == "init":
        init_model(args.fskx, args.models_dir, args.cpu, args.ram, args.gpu, args.dockerfile)
    elif args.command == "fetch":
        fetch_source(args.model_dir, args.out)
    elif args.command == "info":
        print_info(args.model_dir, args.fskx)
    else:
        assemble_context(args.model_dir, args.out_dir, args.fskx)


if __name__ == "__main__":
    main()
