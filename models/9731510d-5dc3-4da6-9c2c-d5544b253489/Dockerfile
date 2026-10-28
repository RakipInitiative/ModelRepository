
# Hand-adjusted: no .Rprofile (keeps the dated CRAN snapshot of the rocker image); gsl 2.1-6 from the CRAN archive (2.1-7/7.1 do not configure against GSL 2.4, 2.1-8 needs R >= 4; install_version cannot see releases newer than the image's CRAN snapshot).
FROM --platform=linux/amd64 rocker/tidyverse:3.6.3-ubuntu18.04

RUN apt-get update && apt-get install -y curl
RUN apt install cmake -y

ADD . /containerscripts
ADD fskx /fskx
# Download the latest Miniconda installer
RUN curl -sSL https://repo.anaconda.com/miniconda/Miniconda3-py38_23.5.0-3-Linux-x86_64.sh -o /miniconda.sh
RUN bash /miniconda.sh -b -p $HOME/miniconda
ENV PATH="/root/miniconda/bin:${PATH}"
RUN conda init
RUN conda env create -f /containerscripts/environment.yml
RUN apt-get update && apt-get install -y libgsl-dev
 RUN R -e "install.packages('remotes')" 
RUN R -e "install.packages('jsonlite')" 
RUN R -e "require(remotes)" -e "install_version('ggplot2', version = '3.3.0')" 
RUN R -e "require(remotes)" -e "install_version('reshape2', version = '1.4.3')" 
RUN R -e "require(remotes)" -e "install_version('MASS', version = '7.3-49')" 
RUN R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/gsl/gsl_2.1-6.tar.gz', repos = NULL, type = 'source')" 
RUN R -e "missing <- setdiff(c('remotes', 'jsonlite', 'ggplot2', 'reshape2', 'MASS', 'gsl'), rownames(installed.packages())); if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))"
ENTRYPOINT . ./containerscripts/entrypoint.sh
