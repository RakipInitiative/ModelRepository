# Shared base image for Python 3.12 models (prototype, not yet built by the workflow).
# Same idea and build call as bases/r-3.6.3/Dockerfile (read its header for the why): the parts that every
# model of this family repeated are built ONCE, a model's Dockerfile starts FROM this image and only adds its
# own packages, the harness scripts and the FSKX model.
#
# Covers the 16 legacy models on python:3.12-slim (2026-10-30 analysis of the CIMS build history). Their pip
# core was pandas, numpy and matplotlib in all 16, openpyxl and seaborn in 12; the LLM client libraries
# (openai, anthropic, mistralai, google-genai, ...) were in about 10 and stay a model-level addition (or a
# second-tier base later). Python is a family of its own per minor version: models that need another Python
# (3.9, 3.10, 3.11) or system libraries (gdal for geopandas) get their own base or stay on a plain FROM.
#
# Build context = the harness/ directory (needs environment.yml only):
#   docker buildx build --platform linux/amd64 --load -t <registry>/fskx/base/python-3.12:<n> \
#     -f bases/python-3.12/Dockerfile harness
# Models refer to it by digest (FROM <registry>/fskx/base/python-3.12@sha256:...).
#
# The scientific core IS pinned, to the set the pinned legacy models already ran with (pandas 2.2.3, numpy 2.2.4,
# matplotlib 3.10.1). Unpinned it resolves to pandas 3.x / numpy 2.5 today, and the first model tried on it (Gravity,
# 7c962507, written for pandas 2) failed with a TypeError on a string column. A model that needs other versions
# installs them in its own Dockerfile; a changed pin means a new base version and a re-test of its models.
#
# Differences to the per-model steps this replaces: no .Rprofile (no R here), the Miniconda installer is deleted in
# the same RUN, conda's package cache is cleaned, pip runs with --no-cache-dir.
FROM python:3.12-slim

RUN apt-get update && apt-get install -y curl cmake

RUN curl -sSL https://repo.anaconda.com/miniconda/Miniconda3-py38_23.5.0-3-Linux-x86_64.sh -o /miniconda.sh \
 && bash /miniconda.sh -b -p $HOME/miniconda \
 && rm /miniconda.sh
ENV PATH="/root/miniconda/bin:${PATH}"
RUN conda init

# The harness environment (python 3.10 + boto3 etc.); the model image copies the harness scripts later.
COPY environment.yml /containerscripts/environment.yml
RUN conda env create -f /containerscripts/environment.yml && conda clean -afy

RUN /usr/local/bin/pip install --no-cache-dir \
    pandas==2.2.3 numpy==2.2.4 matplotlib==3.10.1 openpyxl==3.1.5 seaborn==0.13.2
