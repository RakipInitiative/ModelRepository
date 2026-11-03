# Converted from the legacy Dockerfile built 2025-09-24 by tools/legacy_to_base.py (R 4.5 mode); build and smoke-test it, then adjust by hand.
# On the shared base image bases/r-4.5 (lean rocker/r-ver 4.5.1 + curl, cmake, runtime libraries, Miniconda, the harness
# conda environment, remotes, jsonlite); the base is named by digest. The environment is reproduced from a date: CRAN is the
# Posit Package Manager snapshot of 2025-09-24, the versions of packages.json are installed from it and checked, a package the
# model only names comes in the version of that day.
# CHECK: packages.json declares stat4 4.1.2, left out (--drop stat4)
FROM harbor.cloudferro-dev.risk-ai-cloud.com/fskx-base/r-4.5@sha256:7c4a3133bd4b4a65c7fd6f966682ac51b89188b8506d574299975f5e321c7c5f

RUN echo "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/noble/2025-09-24'))" > /.Rprofile

RUN R -e "require(remotes)" -e "install_version('bbmle', version = '1.0.25.1', upgrade = 'never')" \
 && R -e "require(remotes)" -e "install_version('binom', version = '1.1.1.1', upgrade = 'never')" \
 && R -e "require(remotes)" -e "install_version('boot', version = '1.3.28', upgrade = 'never')" \
 && R -e "require(remotes)" -e "install_version('car', version = '3.1.3', upgrade = 'never')"
RUN R -e "missing <- setdiff(c('bbmle', 'binom', 'boot', 'car'), rownames(installed.packages())); if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))"
# The environment declared in the model's packages.json (and the legacy pins): the build fails if an installed version differs.
RUN R -e "want <- c(bbmle = '1.0.25.1', binom = '1.1.1.1', boot = '1.3.28', car = '3.1.3'); ip <- installed.packages(); got <- unname(ip[match(names(want), rownames(ip)), 'Version']); ok <- !is.na(got) & vapply(seq_along(want), function(i) !is.na(got[i]) && numeric_version(got[i]) == numeric_version(want[i]), logical(1)); got[is.na(got)] <- 'missing'; names(got) <- names(want); bad <- names(want)[!ok]; if (length(bad) > 0) stop(paste('versions differ from packages.json:', paste(bad, got[bad], 'wanted', want[bad], collapse = '; ')))"
# Every named package must load (a missing system library shows up here, not when the model runs).
RUN R -e "invisible(lapply(c('bbmle', 'binom', 'boot', 'car'), library, character.only = TRUE))"

ADD . /containerscripts
ADD fskx /fskx
ENTRYPOINT . ./containerscripts/entrypoint.sh
