# Converted from the legacy Dockerfile built 2025-03-14 by tools/legacy_to_base.py; build and smoke-test it, then adjust by hand.
# On the shared base image bases/r-3.6.3 (rocker/tidyverse 3.6.3-ubuntu18.04 + curl, cmake, Miniconda, the harness
# conda environment, remotes, jsonlite); the base is named by digest.
FROM harbor.cloudferro-dev.risk-ai-cloud.com/fskx-base/r-3.6.3@sha256:6df097e61195a14e57e860d27ce605d7c6c4ff81388115024f555dbeddb0a799

RUN R -e "require(remotes)" -e "install_version('gridGraphics', version = '0.5-0')"
RUN R -e "require(remotes)" -e "install_version('gridExtra', version = '2.3')"
RUN R -e "require(remotes)" -e "install_version('distr', version = '2.8.0')"
RUN R -e "missing <- setdiff(c('gridGraphics', 'gridExtra', 'distr'), rownames(installed.packages())); if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))"
# The environment declared in the model's packages.json: the build fails if an installed version differs.
RUN R -e "want <- c(distr = '2.8.0', gridExtra = '2.3', gridGraphics = '0.5-0'); ip <- installed.packages(); got <- unname(ip[match(names(want), rownames(ip)), 'Version']); got[is.na(got)] <- 'missing'; names(got) <- names(want); bad <- names(want)[got != want]; if (length(bad) > 0) stop(paste('versions differ from packages.json:', paste(bad, got[bad], 'wanted', want[bad], collapse = '; ')))"

ADD . /containerscripts
ADD fskx /fskx
ENTRYPOINT . ./containerscripts/entrypoint.sh
