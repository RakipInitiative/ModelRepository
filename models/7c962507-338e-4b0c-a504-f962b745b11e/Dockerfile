# Gravity model on the shared Python base (bases/python-3.12). Legacy: FROM python:3.9 with unpinned pip installs (built 2025-01-21).
# pandas, numpy, matplotlib and openpyxl come from the base; the model's own packages are pinned here.
# The outputs are files (storesFile, flowFile, populationFile): the harness copies them to /data (HARNESS_VERSION 2).
FROM harbor.cloudferro-dev.risk-ai-cloud.com/fskx-base/python-3.12@sha256:d17a7d550130c2e19a40235282be689fb70ed7d22fadc31583092b3335a630b6

RUN /usr/local/bin/pip install --no-cache-dir ipfn==1.4.4 scipy==1.15.2 \
 && /usr/local/bin/python -c "import ipfn, scipy, numpy, pandas, matplotlib"

ADD . /containerscripts
ADD fskx /fskx
ENTRYPOINT . ./containerscripts/entrypoint.sh
