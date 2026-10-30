# PROTOTYPE of models/9731510d-5dc3-4da6-9c2c-d5544b253489/Dockerfile on the shared base (bases/r-3.6.3/Dockerfile).
# Not used by the workflow. Built locally with the local base tag below; in a real setup FROM would be
# harbor.cloudferro-dev.risk-ai-cloud.com/fskx/base/r-3.6.3@sha256:<digest>, and the model's build.yaml
# version would be bumped (any Dockerfile change needs a higher version).
# Result (2026-10-30): the model adds 9 MB compressed on top of the base (old image: 907 MB beyond the
# rocker layers); the smoke test passes (tools/smoke_test.py).
FROM fskx-proto/base-r:3.6.3-1

RUN apt-get update && apt-get install -y libgsl-dev
RUN R -e "require(remotes)" -e "install_version('ggplot2', version = '3.3.0')"
RUN R -e "require(remotes)" -e "install_version('reshape2', version = '1.4.3')"
RUN R -e "require(remotes)" -e "install_version('MASS', version = '7.3-49')"
RUN R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/gsl/gsl_2.1-6.tar.gz', repos = NULL, type = 'source')"
RUN R -e "missing <- setdiff(c('remotes', 'jsonlite', 'ggplot2', 'reshape2', 'MASS', 'gsl'), rownames(installed.packages())); if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))"

ADD . /containerscripts
ADD fskx /fskx
ENTRYPOINT . ./containerscripts/entrypoint.sh
