# Converted from the legacy Dockerfile built 2025-09-26 by tools/legacy_to_base.py (R 4.5 mode); build and smoke-test it, then adjust by hand.
# On the shared base image bases/r-4.5-bugs (= bases/r-4.5 + OpenBUGS 3.2.3 compiled once; r-4.5 is lean rocker/r-ver 4.5.1 + curl, cmake, runtime libraries, Miniconda, the harness
# conda environment, remotes, jsonlite); the base is named by digest. The environment is reproduced from a date: CRAN is the
# Posit Package Manager snapshot of 2025-09-26, the versions of packages.json are installed from it and checked, a package the
# model only names comes in the version of that day.
# CHECK: the legacy image installed JAGS, the model does not use it (no rjags/R2jags in its scripts or packages.json): not installed
# CHECK: system packages of the legacy image are not installed (libcurl4-openssl-dev, libfreetype6-dev, libglu1-mesa-dev, libssl-dev, libx11-dev, libxml2-dev): the Posit binaries only need the runtime libraries of the base; the load check at the end fails the build if one is missing
FROM test/r-4.5-bugs

RUN echo "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/noble/2025-09-26'))" > /.Rprofile

RUN R -e "require(remotes)" -e "install_version('R2OpenBUGS', version = '3.2-3.2.1', upgrade = 'never')"
RUN R -e "missing <- setdiff(c('R2OpenBUGS'), rownames(installed.packages())); if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))"
# The environment declared in the model's packages.json (and the legacy pins): the build fails if an installed version differs.
RUN R -e "want <- c(R2OpenBUGS = '3.2-3.2.1'); ip <- installed.packages(); got <- unname(ip[match(names(want), rownames(ip)), 'Version']); ok <- !is.na(got) & vapply(seq_along(want), function(i) !is.na(got[i]) && numeric_version(got[i]) == numeric_version(want[i]), logical(1)); got[is.na(got)] <- 'missing'; names(got) <- names(want); bad <- names(want)[!ok]; if (length(bad) > 0) stop(paste('versions differ from packages.json:', paste(bad, got[bad], 'wanted', want[bad], collapse = '; ')))"
# Every named package must load (a missing system library shows up here, not when the model runs).
RUN R -e "invisible(lapply(c('R2OpenBUGS'), library, character.only = TRUE))"

ADD . /containerscripts
ADD fskx /fskx
ENTRYPOINT . ./containerscripts/entrypoint.sh
