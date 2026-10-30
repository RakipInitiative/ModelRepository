#!/bin/bash
# Runs INSIDE the built base image (tools: docker run --entrypoint bash -v check.sh:/check.sh). Fails on the first problem.
set -e
. /root/miniconda/etc/profile.d/conda.sh && conda activate container-env
python -c "import boto3, requests, libsbml; print('harness env ok, python', __import__('sys').version.split()[0])"
conda deactivate
/usr/local/bin/python - <<'PY'
import sys, pandas, numpy, matplotlib, openpyxl, seaborn
assert sys.version_info[:2] == (3, 12), sys.version
assert pandas.__version__ == "2.2.3" and numpy.__version__ == "2.2.4" and matplotlib.__version__ == "3.10.1", (pandas.__version__, numpy.__version__, matplotlib.__version__)
print("python", sys.version.split()[0], "pandas", pandas.__version__, "numpy", numpy.__version__, "ok")
PY
