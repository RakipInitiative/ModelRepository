
# Hand-adjusted: python:3.5 is gone (Debian buster EOL); packages pinned to mid-2022 (model date).
FROM --platform=linux/amd64 python:3.10

RUN apt-get update && apt-get install -y curl
RUN apt install cmake -y
RUN echo "" > .Rprofile
RUN echo "options(repos = c(CRAN = 'http://cloud.r-project.org', RForge = 'http://R-Forge.R-project.org'))" >> .Rprofile

ADD . /containerscripts
ADD fskx /fskx
# Download the latest Miniconda installer
RUN curl -sSL https://repo.anaconda.com/miniconda/Miniconda3-py38_23.5.0-3-Linux-x86_64.sh -o /miniconda.sh
RUN bash /miniconda.sh -b -p $HOME/miniconda
ENV PATH="/root/miniconda/bin:${PATH}"
RUN conda init
RUN conda env create -f /containerscripts/environment.yml

RUN apt-get update && apt-get install -y libgdal-dev gcc 
RUN /usr/local/bin/pip install "pandas==1.4.2" "numpy==1.22.4" 
RUN /usr/local/bin/pip install "geopandas==0.10.2" "shapely==1.8.2" "fiona==1.8.21" 
RUN /usr/local/bin/pip install "matplotlib==3.5.2" 
RUN /usr/local/bin/pip install descartes 
RUN /usr/local/bin/pip install "networkx==2.8.2" 
RUN /usr/local/bin/pip install openpyxl
ENTRYPOINT . ./containerscripts/entrypoint.sh
