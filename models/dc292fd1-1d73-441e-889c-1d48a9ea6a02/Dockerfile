# Converted from the legacy Dockerfile built 2025-05-12 by tools/legacy_to_base.py; build and smoke-test it, then adjust by hand.
# On the shared base image bases/r-3.6.3 (rocker/tidyverse 3.6.3-ubuntu18.04 + curl, cmake, Miniconda, the harness
# conda environment, remotes, jsonlite); the base is named by digest.
# CHECK: an unpinned install.packages line: on the R 3.6.3 base it resolves from the base's dated CRAN snapshot (2020-04-24), so it is reproducible; pin it if the model needs another version (guide step 5, rule 1)
FROM harbor.cloudferro-dev.risk-ai-cloud.com/fskx-base/r-3.6.3@sha256:6df097e61195a14e57e860d27ce605d7c6c4ff81388115024f555dbeddb0a799

RUN R -e "install.packages('ggplot2')"
RUN R -e "missing <- setdiff(c('ggplot2'), rownames(installed.packages())); if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))"

ADD . /containerscripts
ADD fskx /fskx
ENTRYPOINT . ./containerscripts/entrypoint.sh
