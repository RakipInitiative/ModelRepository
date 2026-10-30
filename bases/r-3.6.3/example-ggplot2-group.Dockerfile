# PROTOTYPE of the Dockerfile of the largest legacy group (98 R models, built 2025-05 .. 2025-09, e.g. 944e51f5-...):
# the legacy tail was exactly these three install_version lines. Not used by the workflow. FROM is the local prototype
# tag; a real one is harbor.cloudferro-dev.risk-ai-cloud.com/fskx-base/r-3.6.3@sha256:<digest>.
#
# The dated CRAN snapshot is REQUIRED on R 3.6.3 (tested 2026-10-30): against today's CRAN, and against any snapshot
# after 2024-11, ggplot2 3.5.1 fails with "package 'cpp11' is not available (for R version 3.6.3)" because cpp11 needs
# R >= 4.0 since 2024-12 (scales 1.4.0, 2025, needs R >= 4.1). The snapshot 2024-11-01 resolves a dependency set that
# works (scales 1.3.0, isoband 0.2.7, vctrs 0.6.5, rlang 1.1.4). Repositories come from /.Rprofile, which R reads
# because the build runs R from /.
FROM fskx-proto/base-r:3.6.3-1

RUN echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/2024-11-01'))" > /.Rprofile

RUN R -e "require(remotes)" -e "install_version('gridExtra', version = '2.3')" \
 && R -e "require(remotes)" -e "install_version('ggplot2', version = '3.5.1')" \
 && R -e "require(remotes)" -e "install_version('nleqslv', version = '3.3.5')"
RUN R -e "missing <- setdiff(c('gridExtra', 'ggplot2', 'nleqslv'), rownames(installed.packages())); if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))"

# ADD . /containerscripts
# ADD fskx /fskx
# ENTRYPOINT . ./containerscripts/entrypoint.sh      (as in the model examples; omitted here, this file is built only to measure the package layers)
