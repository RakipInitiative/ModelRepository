# Converted from the legacy Dockerfile built 2025-03-14 by tools/legacy_to_base.py; build and smoke-test it, then adjust by hand.
# On the shared base image bases/r-3.6.3-bugs (= bases/r-3.6.3 + OpenBUGS 3.2.3 compiled once; r-3.6.3 is rocker/tidyverse 3.6.3-ubuntu18.04 + curl, cmake, Miniconda, the harness
# conda environment, remotes, jsonlite); the base is named by digest.
# CHECK: the legacy lines that apt-installed JAGS and 32-bit libraries and compiled OpenBUGS 3.2.3 are replaced by the bugs base; the model does not use JAGS (checked in its scripts)
# CHECK: packages.json declares R2OpenBUGS 3.2-3.2 but the legacy lines install 3.2-3.2.1: decide which one is right
# CHECK: R2OpenBUGS 3.2-3.2 is installed again at the end: a later legacy line replaces it
FROM harbor.cloudferro-dev.risk-ai-cloud.com/fskx-base/r-3.6.3-bugs@sha256:219c0ad200231a88a175c722a4347ea29f322fca25988097c99bf4503bf9f2b3

RUN R -e "require(remotes)" -e "install_version('R2OpenBUGS', version = '3.2-3.2.1')"
RUN R -e "require(remotes)" -e "install_version('R2OpenBUGS', version = '3.2-3.2', upgrade = 'never')"
RUN R -e "missing <- setdiff(c('R2OpenBUGS'), rownames(installed.packages())); if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))"
# The environment declared in the model's packages.json: the build fails if an installed version differs.
RUN R -e "want <- c(R2OpenBUGS = '3.2-3.2'); ip <- installed.packages(); got <- unname(ip[match(names(want), rownames(ip)), 'Version']); got[is.na(got)] <- 'missing'; names(got) <- names(want); bad <- names(want)[got != want]; if (length(bad) > 0) stop(paste('versions differ from packages.json:', paste(bad, got[bad], 'wanted', want[bad], collapse = '; ')))"

ADD . /containerscripts
ADD fskx /fskx
ENTRYPOINT . ./containerscripts/entrypoint.sh
