# Converted from the legacy Dockerfile built 2025-09-24 by tools/legacy_to_base.py (R 4.5 mode); build and smoke-test it, then adjust by hand.
# On the shared base image bases/r-4.5 (lean rocker/r-ver 4.5.1 + curl, cmake, runtime libraries, Miniconda, the harness
# conda environment, remotes, jsonlite); the base is named by digest. The environment is reproduced from a date: CRAN is the
# Posit Package Manager snapshot of 2025-09-24, the versions of packages.json are installed from it and checked, a package the
# model only names comes in the version of that day.
FROM harbor.cloudferro-dev.risk-ai-cloud.com/fskx-base/r-4.5@sha256:7c4a3133bd4b4a65c7fd6f966682ac51b89188b8506d574299975f5e321c7c5f

RUN echo "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/noble/2025-09-24'))" > /.Rprofile

RUN R -e "install.packages(c('ggplot2'))"
RUN R -e "missing <- setdiff(c('ggplot2'), rownames(installed.packages())); if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))"
# Every named package must load (a missing system library shows up here, not when the model runs).
RUN R -e "invisible(lapply(c('ggplot2'), library, character.only = TRUE))"

ADD . /containerscripts
ADD fskx /fskx
ENTRYPOINT . ./containerscripts/entrypoint.sh
