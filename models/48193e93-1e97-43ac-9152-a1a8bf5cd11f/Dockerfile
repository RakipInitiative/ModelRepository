# Converted from the legacy Dockerfile built 2025-03-14 by tools/legacy_to_base.py; build and smoke-test it, then adjust by hand.
# On the shared base image bases/r-3.6.3-bugs (= bases/r-3.6.3 + OpenBUGS 3.2.3 compiled once; r-3.6.3 is rocker/tidyverse 3.6.3-ubuntu18.04 + curl, cmake, Miniconda, the harness
# conda environment, remotes, jsonlite); the base is named by digest.
# CHECK: an unpinned install.packages line: on the R 3.6.3 base it resolves from the base's dated CRAN snapshot (2020-04-24), so it is reproducible; pin it if the model needs another version (guide step 5, rule 1)
# CHECK: the legacy lines that apt-installed JAGS and 32-bit libraries and compiled OpenBUGS 3.2.3 are replaced by the bugs base; the model does not use JAGS (checked in its scripts)
# CHECK: packages.json declares R2OpenBUGS 3.2-3.2 but the legacy lines install 3.2-3.2.1: decide which one is right
# CHECK: R2OpenBUGS 3.2-3.2 is installed again at the end: a later legacy line replaces it
FROM test/r-3.6.3-bugs

# Dated CRAN snapshot: ggplot2 >= 3.4 does not install from today's CRAN on R 3.6.3 (see bases/r-3.6.3/example-ggplot2-group.Dockerfile).
RUN echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/2024-11-01'))" > /.Rprofile

RUN R -e "install.packages('lattice')"
RUN R -e "install.packages('coda')"
RUN R -e "install.packages('openxlsx')"
RUN R -e "require(remotes)" -e "install_version('R2OpenBUGS', version = '3.2-3.2.1')"
RUN R -e "require(remotes)" -e "install_version('coda', version = '0.19-2')"
RUN R -e "require(remotes)" -e "install_version('lattice', version = '0.20-35')"
RUN R -e "require(remotes)" -e "install_version('openxlsx', version = '4.2.5.1')"
RUN R -e "require(remotes)" -e "install_version('R2OpenBUGS', version = '3.2-3.2', upgrade = 'never')"
RUN R -e "missing <- setdiff(c('lattice', 'coda', 'openxlsx', 'R2OpenBUGS'), rownames(installed.packages())); if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))"
# The environment declared in the model's packages.json: the build fails if an installed version differs.
RUN R -e "want <- c(R2OpenBUGS = '3.2-3.2', coda = '0.19-2', lattice = '0.20-35', openxlsx = '4.2.5.1'); ip <- installed.packages(); got <- unname(ip[match(names(want), rownames(ip)), 'Version']); got[is.na(got)] <- 'missing'; names(got) <- names(want); bad <- names(want)[got != want]; if (length(bad) > 0) stop(paste('versions differ from packages.json:', paste(bad, got[bad], 'wanted', want[bad], collapse = '; ')))"

ADD . /containerscripts
ADD fskx /fskx
ENTRYPOINT . ./containerscripts/entrypoint.sh
