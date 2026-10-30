#!/bin/bash
# Runs INSIDE the built base image (tools: docker run --entrypoint bash -v check.sh:/check.sh). Fails on the first problem.
set -e
. /root/miniconda/etc/profile.d/conda.sh && conda activate container-env
python -c "import boto3, requests, libsbml; print('harness env ok, python', __import__('sys').version.split()[0])"
R -q -e "library(remotes); library(jsonlite); library(ggplot2); cat('R', as.character(getRversion()), 'ok\n')"
