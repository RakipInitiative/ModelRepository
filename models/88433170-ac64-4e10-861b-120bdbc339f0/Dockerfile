# Converted from the legacy Dockerfile built 2025-10-20 by tools/legacy_to_base.py (R 4.5 mode); build and smoke-test it, then adjust by hand.
# On the shared base image bases/r-4.5 (lean rocker/r-ver 4.5.1 + curl, cmake, runtime libraries, Miniconda, the harness
# conda environment, remotes, jsonlite); the base is named by digest. The environment is reproduced from a date: CRAN is the
# Posit Package Manager snapshot of 2025-10-20, the versions of packages.json are installed from it and checked, a package the
# model only names comes in the version of that day.
# CHECK: system packages added by --apt: tk-dev
# CHECK: packages.json names the language 'R 4.4.3', the base is R 4.5.1: decide whether that is acceptable
FROM harbor.cloudferro-dev.risk-ai-cloud.com/fskx-base/r-4.5@sha256:7c4a3133bd4b4a65c7fd6f966682ac51b89188b8506d574299975f5e321c7c5f

RUN echo "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/noble/2025-10-20'))" > /.Rprofile

RUN apt-get update && apt-get install -y --no-install-recommends tk-dev && rm -rf /var/lib/apt/lists/*
RUN R -e "require(remotes)" -e "install_version('data.table', version = '1.17.8', upgrade = 'never')" \
 && R -e "require(remotes)" -e "install_version('dplyr', version = '1.1.4', upgrade = 'never')" \
 && R -e "require(remotes)" -e "install_version('ggplot2', version = '4.0.0', upgrade = 'never')" \
 && R -e "require(remotes)" -e "install_version('mc2d', version = '0.2.1', upgrade = 'never')" \
 && R -e "require(remotes)" -e "install_version('patchwork', version = '1.3.2', upgrade = 'never')" \
 && R -e "require(remotes)" -e "install_version('readxl', version = '1.4.5', upgrade = 'never')" \
 && R -e "require(remotes)" -e "install_version('rriskDistributions', version = '2.1.2', upgrade = 'never')" \
 && R -e "require(remotes)" -e "install_version('tidyr', version = '1.3.1', upgrade = 'never')"
RUN R -e "missing <- setdiff(c('data.table', 'dplyr', 'ggplot2', 'mc2d', 'patchwork', 'readxl', 'rriskDistributions', 'tidyr'), rownames(installed.packages())); if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))"
# The environment declared in the model's packages.json (and the legacy pins): the build fails if an installed version differs.
RUN R -e "want <- c(data.table = '1.17.8', dplyr = '1.1.4', ggplot2 = '4.0.0', mc2d = '0.2.1', patchwork = '1.3.2', readxl = '1.4.5', rriskDistributions = '2.1.2', tidyr = '1.3.1'); ip <- installed.packages(); got <- unname(ip[match(names(want), rownames(ip)), 'Version']); ok <- !is.na(got) & vapply(seq_along(want), function(i) !is.na(got[i]) && numeric_version(got[i]) == numeric_version(want[i]), logical(1)); got[is.na(got)] <- 'missing'; names(got) <- names(want); bad <- names(want)[!ok]; if (length(bad) > 0) stop(paste('versions differ from packages.json:', paste(bad, got[bad], 'wanted', want[bad], collapse = '; ')))"
# Every named package must load (a missing system library shows up here, not when the model runs).
RUN R -e "invisible(lapply(c('data.table', 'dplyr', 'ggplot2', 'mc2d', 'patchwork', 'readxl', 'rriskDistributions', 'tidyr'), library, character.only = TRUE))"

ADD . /containerscripts
ADD fskx /fskx
ENTRYPOINT . ./containerscripts/entrypoint.sh
