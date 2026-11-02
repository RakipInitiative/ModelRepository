#!/bin/bash
# Runs INSIDE the built base image (tools: docker run --entrypoint bash -v check.sh:/check.sh). Fails on the first problem.
set -e
. /root/miniconda/etc/profile.d/conda.sh && conda activate container-env
python -c "import boto3, requests, libsbml; print('harness env ok, python', __import__('sys').version.split()[0])"
R -q -e "library(remotes); library(jsonlite); stopifnot(getRversion() == '4.5.1'); cat('R', as.character(getRversion()), 'ok\n')"
# the detectCores hook: capped with FSKX_CPUS (also through ::, in options and in mclapply), untouched without it
FSKX_CPUS=2 Rscript -e "options(mc.cores = parallel::detectCores()); stopifnot(parallel::detectCores() == 2, getOption('mc.cores') == 2); library(parallel); stopifnot(detectCores() == 2, length(unique(unlist(mclapply(1:8, function(i) Sys.getpid(), mc.cores = getOption('mc.cores'))))) == 2)"
Rscript -e "stopifnot(parallel::detectCores() >= 1)"
echo "detectCores hook ok"
# the runtime libraries: a graphics device and xml/curl based packages from the dated snapshot flow
Rscript -e "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/noble/2025-10-20')); install.packages(c('ggplot2', 'xml2', 'curl', 'readxl', 'ragg')); library(ggplot2); library(ragg); library(xml2); library(curl); library(readxl); png(tempfile()); print(ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) + geom_point()); dev.off(); cat('packages from a dated snapshot ok\n')"
