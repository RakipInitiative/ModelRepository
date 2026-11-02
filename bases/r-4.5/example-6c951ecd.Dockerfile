# Real model on the r-4.5 base (6c951ecd, legacy image: rocker/r2u:latest built 2025-10-20). The environment is reproduced from a date:
# CRAN is pointed to the Posit Package Manager snapshot of the day the legacy image was built (the last evidence of a working
# environment), the versions declared in the model's packages.json are installed from it, and the build fails if an installed
# version differs from the declared one. Packages the model only names (no version) come in the version of that day.
# FROM is the local prototype tag; a real one is harbor.cloudferro-dev.risk-ai-cloud.com/fskx-base/r-4.5@sha256:<digest>.
FROM test/base-r-4.5

RUN echo "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/noble/2025-10-20'))" > /.Rprofile

RUN R -e "require(remotes)" -e "install_version('ggplot2', version = '3.5.1')" \
 && R -e "require(remotes)" -e "install_version('gridExtra', version = '2.3')"
RUN R -e "want <- c(ggplot2 = '3.5.1', gridExtra = '2.3'); ip <- installed.packages(); got <- unname(ip[match(names(want), rownames(ip)), 'Version']); got[is.na(got)] <- 'missing'; names(got) <- names(want); bad <- names(want)[got != want]; if (length(bad) > 0) stop(paste('versions differ from packages.json:', paste(bad, got[bad], 'wanted', want[bad], collapse = '; ')))"

ADD . /containerscripts
ADD fskx /fskx
ENTRYPOINT . ./containerscripts/entrypoint.sh
