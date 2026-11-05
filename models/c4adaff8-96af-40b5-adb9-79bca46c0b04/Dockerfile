# Rebuilt on the shared Python base (bases/python-3.12). Legacy: FROM python:3.12.9 with pip installs of
# matplotlib==3.10.1, numpy==2.2.4, pandas==2.2.3 (built 2025-03-28); the base ships exactly these versions,
# so the model adds nothing. The check below fails the build if the base ever drifts from the declared versions.
FROM harbor.cloudferro-dev.risk-ai-cloud.com/fskx-base/python-3.12@sha256:d17a7d550130c2e19a40235282be689fb70ed7d22fadc31583092b3335a630b6

RUN /usr/local/bin/python -c "import matplotlib, numpy, pandas; assert (matplotlib.__version__, numpy.__version__, pandas.__version__) == ('3.10.1', '2.2.4', '2.2.3'), (matplotlib.__version__, numpy.__version__, pandas.__version__)"

ADD . /containerscripts
ADD fskx /fskx
ENTRYPOINT . ./containerscripts/entrypoint.sh
