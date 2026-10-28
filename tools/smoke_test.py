#!/usr/bin/env python3
"""Smoke test for a model image: run it with its default parameters and check that it produced results.

  smoke_test.py <image> [--timeout 900] [--keep DIR]

Language-agnostic on purpose: the image is run the way the Model Execution Service runs it
(JSON config at /workspace/config, results in /data) and must write a valid, non-empty
/data/output.json. The container exit code says nothing (the harness exits 0 when the
model script fails), and the S3 upload is expected to fail here (no credentials).
"""
import argparse
import json
import os
import shutil
import subprocess
import sys
import tempfile

CONFIG = {
    "command": "simulate",
    "parameters": {"simulation_id": "smoke-test", "plot_type": "png"},
    "result_bucket_s3": "smoke-test-no-upload",
}


def run_smoke_test(image, timeout=900, keep=None, platform="linux/amd64"):
    work = keep or tempfile.mkdtemp(prefix="smoke-")
    data_dir = os.path.join(work, "data")
    os.makedirs(data_dir, exist_ok=True)
    config_path = os.path.join(work, "config")
    with open(config_path, "w") as f:
        json.dump(CONFIG, f)
    cmd = ["docker", "run", "--rm", "--platform", platform,
           "-e", "AWS_EC2_METADATA_DISABLED=true",
           "-v", f"{config_path}:/workspace/config:ro",
           "-v", f"{data_dir}:/data", image]
    problems = []
    try:
        proc = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)
        if proc.returncode != 0:
            problems.append(f"container exited with {proc.returncode}")
    except subprocess.TimeoutExpired:
        problems.append(f"timeout after {timeout} s")

    output_path = os.path.join(data_dir, "output.json")
    if not os.path.isfile(output_path):
        problems.append("/data/output.json was not written")
    else:
        try:
            with open(output_path) as f:
                if not json.load(f):
                    problems.append("/data/output.json is empty")
        except ValueError as e:
            problems.append(f"/data/output.json is not valid JSON: {e}")

    files = sorted(os.listdir(data_dir))
    if problems:
        log = os.path.join(data_dir, "simulation_log.log")
        if os.path.isfile(log):
            with open(log, errors="replace") as f:
                print("--- simulation_log.log (tail) ---\n" + "".join(f.readlines()[-30:]), file=sys.stderr)
    if not keep:
        shutil.rmtree(work, ignore_errors=True)
    return problems, files


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("image")
    parser.add_argument("--timeout", type=int, default=900)
    parser.add_argument("--keep", help="keep config and results in this directory")
    args = parser.parse_args()
    problems, files = run_smoke_test(args.image, args.timeout, args.keep)
    print(f"files in /data: {', '.join(files) or '(none)'}")
    if problems:
        print("SMOKE TEST FAILED: " + "; ".join(problems), file=sys.stderr)
        sys.exit(1)
    print("smoke test passed")


if __name__ == "__main__":
    main()
