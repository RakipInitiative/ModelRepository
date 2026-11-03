# Converted from the legacy Dockerfile built 2025-10-08 by tools/legacy_to_base.py (R 4.5 mode); build and smoke-test it, then adjust by hand.
# On the shared base image bases/r-4.5 (lean rocker/r-ver 4.5.1 + curl, cmake, runtime libraries, Miniconda, the harness
# conda environment, remotes, jsonlite); the base is named by digest. The environment is reproduced from a date: CRAN is the
# Posit Package Manager snapshot of 2025-10-08, the versions of packages.json are installed from it and checked, a package the
# model only names comes in the version of that day.
# CHECK: system packages of the legacy image are not installed (build-essential, g++, gfortran, libblas-dev, libcurl4-openssl-dev, liblapack-dev, libssl-dev, libv8-dev...): the Posit binaries only need the runtime libraries of the base; the load check at the end fails the build if one is missing
# CHECK: legacy line not carried over: RUN mkdir -p /root/.R && echo "\nCXX17FLAGS=-O3 -march=native -mtune=native -fPIC" >> /root/.R/Makevars && ech...
FROM harbor.cloudferro-dev.risk-ai-cloud.com/fskx-base/r-4.5@sha256:7c4a3133bd4b4a65c7fd6f966682ac51b89188b8506d574299975f5e321c7c5f

RUN echo "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/noble/2025-10-08'))" > /.Rprofile

RUN R -e "install.packages(c('cowplot', 'dplyr', 'furrr', 'ggdist', 'glue', 'gt', 'mc2d', 'pacman', 'patchwork', 'readxl', 'rlang', 'scales', 'tidyverse', 'writexl'))"
RUN R -e "missing <- setdiff(c('cowplot', 'dplyr', 'furrr', 'ggdist', 'glue', 'gt', 'mc2d', 'pacman', 'patchwork', 'readxl', 'rlang', 'scales', 'tidyverse', 'writexl'), rownames(installed.packages())); if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))"
# Every named package must load (a missing system library shows up here, not when the model runs).
RUN R -e "invisible(lapply(c('cowplot', 'dplyr', 'furrr', 'ggdist', 'glue', 'gt', 'mc2d', 'pacman', 'patchwork', 'readxl', 'rlang', 'scales', 'tidyverse', 'writexl'), library, character.only = TRUE))"

ADD . /containerscripts
ADD fskx /fskx
ENTRYPOINT . ./containerscripts/entrypoint.sh
