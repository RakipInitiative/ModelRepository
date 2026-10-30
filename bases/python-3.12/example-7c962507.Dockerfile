# PROTOTYPE of a model Dockerfile on the shared Python base (bases/python-3.12/Dockerfile), here for the
# Gravity model 7c962507 (legacy: FROM python:3.9, unpinned pip, 1185 MB compressed in Harbor, one 811 MB layer).
# Not used by the workflow. FROM is the local prototype tag; a real one would be
# harbor.cloudferro-dev.risk-ai-cloud.com/fskx/base/python-3.12@sha256:<digest>, and build.yaml needs a higher version.
# Result (2026-10-30): base 640 MB + this model 46 MB compressed (ipfn + scipy + model). The model runs the same as
# in the old-style image (same plot, same empty output.json: its outputs are files, so tools/smoke_test.py, which
# demands a non-empty output.json, fails for it in both images).
# The model's own packages are pinned too: unpinned they would follow the newest releases at build time.
FROM fskx-proto/base-python:3.12-1

RUN /usr/local/bin/pip install --no-cache-dir ipfn==1.4.4 scipy==1.15.2

ADD . /containerscripts
ADD fskx /fskx
ENTRYPOINT . ./containerscripts/entrypoint.sh
