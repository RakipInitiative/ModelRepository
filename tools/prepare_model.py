#!/usr/bin/env python3
"""Curator and CI helper for models/<model_id>/ (Dockerfile + build.yaml only).

The FSKX file itself is not stored in git. It lives in the model-lookup-service (S3) and is
downloaded when needed. The folder name is the model id.

  prepare_model.py init <model_id | file.fskx> [--dockerfile my.Dockerfile] [--models-dir models]
                        [--cpu 1] [--ram 1] [--gpu 0]
      Creates models/<model_id>/ with Dockerfile and build.yaml. A model id is downloaded from
      the lookup service, a file is read locally. With --dockerfile the curator's hand-written
      Dockerfile is used as is. Without it, a Dockerfile is generated from the packages in the
      FSKX file; this is only a starting point and often needs manual fixes. The committed
      Dockerfile is the source of truth: the build workflow never regenerates it.

  prepare_model.py fetch <model_id> <out.fskx>
      Downloads the original FSKX file from the lookup service and prints its sha256.

  prepare_model.py info models/<model_id> --fskx file.fskx
      Prints key=value lines (name, version, cpu, ram, gpu, harness_version, fskx_sha256) for
      the build workflow. Fails if the folder name differs from the model identifier.

  prepare_model.py context models/<model_id> <out_dir> --fskx file.fskx
      Assembles the Docker build context: the harness files at the top level and the unpacked
      model in fskx/. Build with
      docker buildx build --platform linux/amd64 -f models/<model_id>/Dockerfile <out_dir>

Access to the lookup service:
  FSKX_API_KEY set   sent as X-API-KEY to the protected endpoint (works for uncurated models;
                     the build workflow uses a key from the GitHub secret, a curator can create
                     their own key)
  not set            public endpoint, works for curated models only
Other setting: FSKX_LOOKUP_URL.

Needs Python >= 3.10 for generating a Dockerfile.
"""
import argparse
import hashlib
import json
import os
import re
import shutil
import sys
import tempfile
import urllib.error
import urllib.request
import zipfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from FskxFile import FskxFile
from fskx_dockerfile import dockerfile_from_model

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
HARNESS_DIR = os.path.join(REPO_ROOT, "harness")

DEFAULT_LOOKUP_URL = "https://api-gateway.cloudferro-dev.risk-ai-cloud.com/model-lookup-service"
ORIGINAL_FILE_NAME = "fskx_file.fskx"
MODEL_ID_PATTERN = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]*$")


def extract_fskx(fskx_path, dest_dir):
    with zipfile.ZipFile(fskx_path, "r") as zip_ref:
        zip_ref.extractall(dest_dir)


def build_yaml(version=1, cpu=1, ram=1, gpu=0):
    return f"version: {version}\ncpu: {cpu}\nram: {ram}\ngpu: {gpu}\n"


def read_general_information(fskx_path):
    with zipfile.ZipFile(fskx_path) as z:
        with z.open("metaData.json") as f:
            return json.load(f)["generalInformation"]


def sha256_file(path):
    digest = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


# ---- download from the model-lookup-service ------------------------------------------------

def fetch_model(model_id, out_path):
    """Downloads <model_id>/fskx_file.fskx; returns its sha256."""
    if not MODEL_ID_PATTERN.match(model_id):
        sys.exit(f"invalid model id: {model_id}")
    base = os.environ.get("FSKX_LOOKUP_URL", DEFAULT_LOOKUP_URL).rstrip("/")
    if not base.startswith("https://"):
        sys.exit("FSKX_LOOKUP_URL must be an https URL.")
    api_key = os.environ.get("FSKX_API_KEY")
    if api_key:
        url = f"{base}/models/{model_id}/files/{ORIGINAL_FILE_NAME}"
        request = urllib.request.Request(url, headers={"X-API-KEY": api_key})
        print("using the protected endpoint with an API key")
    else:
        url = f"{base}/public/models/{model_id}/files/{ORIGINAL_FILE_NAME}"
        request = urllib.request.Request(url)
        print("FSKX_API_KEY is not set: using the public endpoint (curated models only)")
    try:
        with urllib.request.urlopen(request, timeout=120) as response, open(out_path, "wb") as out:
            shutil.copyfileobj(response, out, 1 << 20)
    except urllib.error.HTTPError as e:
        hint = {401: "API key invalid or expired", 403: "no permission",
                404: "model not found or (public endpoint) not curated"}.get(e.code, "")
        sys.exit(f"download failed: HTTP {e.code} for {url} {hint}".strip())
    except urllib.error.URLError as e:
        sys.exit(f"download failed for {url}: {e.reason}")
    if not zipfile.is_zipfile(out_path):
        os.remove(out_path)
        sys.exit(f"{url} did not return an FSKX (zip) file; a login redirect means the API key is missing.")
    return sha256_file(out_path)


# ---- model folder ----------------------------------------------------------------------------

def init_model(source, models_dir, cpu, ram, gpu, dockerfile_path=None):
    """source: path to a local .fskx, or a model id to download."""
    with tempfile.TemporaryDirectory() as tmp:
        if os.path.isfile(source):
            fskx_path = source
        else:
            fskx_path = os.path.join(tmp, "model.fskx")
            fetch_model(source, fskx_path)
        digest = sha256_file(fskx_path)
        if dockerfile_path:
            # Hand-written Dockerfile: no generator, so no dependency scan is needed.
            with open(dockerfile_path) as f:
                dockerfile = f.read()
            info = read_general_information(fskx_path)
            model_id, model_name, how = info["identifier"], info["name"], "hand-written Dockerfile"
        else:
            extracted = os.path.join(tmp, "extracted")
            extract_fskx(fskx_path, extracted)
            model = FskxFile(extracted)
            dockerfile = dockerfile_from_model(model)
            model_id, model_name = model.model_id, model.model_name
            how = f"generated for {model.programming_language} {model.programming_version}"

    model_dir = os.path.join(models_dir, model_id)
    if os.path.exists(model_dir):
        sys.exit(f"{model_dir} already exists; refusing to overwrite.")
    os.makedirs(model_dir)
    with open(os.path.join(model_dir, "Dockerfile"), "w") as f:
        f.write(dockerfile)
    with open(os.path.join(model_dir, "build.yaml"), "w") as f:
        f.write(build_yaml(cpu=cpu, ram=ram, gpu=gpu))
    print(f"{model_name} ({how}), fskx sha256 {digest}")
    print(f"created {model_dir}: Dockerfile, build.yaml (the FSKX file is not stored here)")
    return model_dir


def read_simple_yaml(path):
    """Flat `key: value` files (build.yaml); no PyYAML needed on the runner."""
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


def check_fskx(model_dir, fskx_path):
    if not os.path.isfile(fskx_path):
        sys.exit(f"{fskx_path} not found (run fetch first).")
    info = read_general_information(fskx_path)
    folder = os.path.basename(os.path.normpath(model_dir))
    if info["identifier"] != folder:
        sys.exit(f"folder name {folder} differs from the model identifier {info['identifier']}.")
    return info


def print_info(model_dir, fskx_path):
    info = check_fskx(model_dir, fskx_path)
    build = read_build_yaml(model_dir)
    with open(os.path.join(HARNESS_DIR, "HARNESS_VERSION")) as f:
        harness_version = f.read().strip()
    name = " ".join(info["name"].split())
    for key, value in [("name", name), ("version", build["version"]), ("cpu", build["cpu"]),
                       ("ram", build["ram"]), ("gpu", build["gpu"]), ("harness_version", harness_version),
                       ("fskx_sha256", sha256_file(fskx_path))]:
        print(f"{key}={value}")


def assemble_context(model_dir, out_dir, fskx_path):
    check_fskx(model_dir, fskx_path)
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

    p_init = sub.add_parser("init", help="create models/<model_id>/ (Dockerfile + build.yaml)")
    p_init.add_argument("source", help="model id (downloaded) or path to a local .fskx file")
    p_init.add_argument("--dockerfile", help="use this hand-written Dockerfile instead of generating one")
    p_init.add_argument("--models-dir", default=os.path.join(REPO_ROOT, "models"))
    p_init.add_argument("--cpu", type=int, default=1)
    p_init.add_argument("--ram", type=int, default=1, help="GB")
    p_init.add_argument("--gpu", type=int, default=0)

    p_fetch = sub.add_parser("fetch", help="download the FSKX file of a model from the lookup service")
    p_fetch.add_argument("model_id")
    p_fetch.add_argument("out")

    p_info = sub.add_parser("info", help="print build metadata as key=value lines")
    p_info.add_argument("model_dir")
    p_info.add_argument("--fskx", required=True)

    p_ctx = sub.add_parser("context", help="assemble the Docker build context")
    p_ctx.add_argument("model_dir")
    p_ctx.add_argument("out_dir")
    p_ctx.add_argument("--fskx", required=True)

    args = parser.parse_args()
    if args.command == "init":
        init_model(args.source, args.models_dir, args.cpu, args.ram, args.gpu, args.dockerfile)
    elif args.command == "fetch":
        digest = fetch_model(args.model_id, args.out)
        print(f"downloaded {args.model_id} ({os.path.getsize(args.out)} bytes, sha256 {digest})")
    elif args.command == "info":
        print_info(args.model_dir, args.fskx)
    else:
        assemble_context(args.model_dir, args.out_dir, args.fskx)


if __name__ == "__main__":
    main()
