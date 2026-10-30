# Shared base image for the R 3.6.3 models (prototype, not yet built by the workflow).
#
# Why: layers are shared on a node (and in Harbor) only when they are byte-identical AND everything below
# them is identical. Re-running the same RUN lines in every model build gives different layers each time,
# and the legacy builder squashed everything after FROM into one ~600 MB layer per model, so a node paid
# ~720 MB for every additional model (scripts/layer_overlap.py, 2026-10-30). Here the parts that every
# R 3.6.3 model repeated (apt tools, Miniconda, the harness conda environment, remotes + jsonlite) are built
# ONCE; a model's Dockerfile starts FROM this image and only adds its own packages, the harness scripts
# and the FSKX model on top.
#
# Build context = the harness/ directory (needs environment.yml only):
#   docker buildx build --platform linux/amd64 --load -t <registry>/fskx/base/r-3.6.3:<n> \
#     -f bases/r-3.6.3/Dockerfile harness
# Models refer to it by digest (FROM <registry>/fskx/base/r-3.6.3@sha256:...), so a rebuilt base changes no
# existing model until its Dockerfile is bumped.
#
# Differences to the per-model steps this replaces (same installed software, same paths):
#  - the Miniconda installer is deleted in the same RUN (the old image kept the ~100 MB file in a layer)
#  - conda's package cache is cleaned after `conda env create`
FROM --platform=linux/amd64 rocker/tidyverse:3.6.3-ubuntu18.04

RUN apt-get update && apt-get install -y curl cmake

RUN curl -sSL https://repo.anaconda.com/miniconda/Miniconda3-py38_23.5.0-3-Linux-x86_64.sh -o /miniconda.sh \
 && bash /miniconda.sh -b -p $HOME/miniconda \
 && rm /miniconda.sh
ENV PATH="/root/miniconda/bin:${PATH}"
RUN conda init

# The harness environment (python 3.10 + boto3 etc.); the model image copies the harness scripts later.
COPY environment.yml /containerscripts/environment.yml
RUN conda env create -f /containerscripts/environment.yml && conda clean -afy

RUN R -e "install.packages('remotes')" -e "install.packages('jsonlite')" \
 && R -e "missing <- setdiff(c('remotes', 'jsonlite'), rownames(installed.packages())); if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))"
