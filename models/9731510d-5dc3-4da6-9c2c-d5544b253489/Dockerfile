
# On the shared base image bases/r-3.6.3 (rocker/tidyverse 3.6.3-ubuntu18.04 + curl, cmake, Miniconda, the harness
# conda environment, remotes, jsonlite); the base is named by digest. Only the model-specific packages are installed here.
# Hand-adjusted: no .Rprofile (keeps the dated CRAN snapshot of the rocker image); gsl 2.1-6 from the CRAN archive (2.1-7/7.1 do not configure against GSL 2.4, 2.1-8 needs R >= 4; install_version cannot see releases newer than the image's CRAN snapshot).
FROM harbor.cloudferro-dev.risk-ai-cloud.com/fskx-base/r-3.6.3@sha256:6df097e61195a14e57e860d27ce605d7c6c4ff81388115024f555dbeddb0a799

RUN apt-get update && apt-get install -y libgsl-dev
RUN R -e "require(remotes)" -e "install_version('ggplot2', version = '3.3.0')" 
RUN R -e "require(remotes)" -e "install_version('reshape2', version = '1.4.3')" 
RUN R -e "require(remotes)" -e "install_version('MASS', version = '7.3-49')" 
RUN R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/gsl/gsl_2.1-6.tar.gz', repos = NULL, type = 'source')" 
RUN R -e "missing <- setdiff(c('remotes', 'jsonlite', 'ggplot2', 'reshape2', 'MASS', 'gsl'), rownames(installed.packages())); if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))"

ADD . /containerscripts
ADD fskx /fskx
ENTRYPOINT . ./containerscripts/entrypoint.sh
