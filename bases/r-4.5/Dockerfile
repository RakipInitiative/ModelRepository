# Shared base image for the R 4.5 models: LEAN, no tidyverse. Used for models whose environment has to be reproduced
# from a date (package versions of the model's packages.json, everything else as of the date the model was built).
#
# Why lean: rocker/tidyverse:4.5 is a floating tag (R 4.5.3 with the packages of its own snapshot date) and rocker/r2u
# installs the newest CRAN binaries from apt. Neither can reproduce an older package state. Here the base holds only R
# (rocker/r-ver, pinned by digest), the runtime libraries Posit's binary packages need, Miniconda and the harness
# environment. A model's Dockerfile points CRAN to a dated Posit Package Manager snapshot and installs its packages
# from there (bases/r-4.5/example-dated-snapshot.Dockerfile); that gives binaries (fast, small) of exactly that day.
#
# Build context = the harness/ directory (needs environment.yml only), like bases/r-3.6.3.
#
# The R profile hook caps parallel::detectCores() at the cores reserved for the simulation. Models size their worker
# pools from detectCores() (options(mc.cores = parallel::detectCores()), plan("multisession", workers = ...)), which
# reports the cores of the whole node; MC_CORES cannot change that because the model code sets the option itself.
# MES passes the reserved cores as FSKX_CPUS (kubernetes_setup.simulation_cpu_env); without it nothing changes.
FROM --platform=linux/amd64 rocker/r-ver:4.5.1@sha256:03b023fbf7b1b24ac1bb8b2ac5fd7e15a767e67b40ff50c155e328110981c2aa

# curl, cmake: like the other bases (some model packages build with cmake). The libraries are the runtime side of the
# packages most models use (graphics devices, xml, curl, fonts, images); a dev package is added by a model that compiles.
RUN apt-get update && apt-get install -y --no-install-recommends \
        curl cmake ca-certificates \
        libcurl4t64 libssl3t64 libxml2 libfontconfig1 libfreetype6 libpng16-16t64 libjpeg-turbo8 libtiff6 \
        libharfbuzz0b libfribidi0 libicu74 libwebp7 libwebpmux3 libwebpdemux2 \
 && rm -rf /var/lib/apt/lists/*

RUN curl -sSL https://repo.anaconda.com/miniconda/Miniconda3-py38_23.5.0-3-Linux-x86_64.sh -o /miniconda.sh \
 && bash /miniconda.sh -b -p $HOME/miniconda \
 && rm /miniconda.sh
ENV PATH="/root/miniconda/bin:${PATH}"
RUN conda init

# The harness environment (python 3.10 + boto3 etc.); the model image copies the harness scripts later.
COPY environment.yml /containerscripts/environment.yml
RUN conda env create -f /containerscripts/environment.yml && conda clean -afy

RUN R -e "install.packages('remotes')" -e "install.packages('jsonlite')" \
 && R -e "missing <- setdiff(c('remotes', 'jsonlite'), rownames(installed.packages())); if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))"

RUN cat >> /usr/local/lib/R/etc/Rprofile.site <<'RPROFILE'

# FSKX: cap parallel::detectCores() at the cores reserved for the simulation (env FSKX_CPUS, set by MES).
if (nzchar(Sys.getenv("FSKX_CPUS"))) {
  setHook(packageEvent("parallel", "onLoad"), function(...) {
    ns <- asNamespace("parallel")
    original <- get("detectCores", envir = ns)
    cap <- as.integer(Sys.getenv("FSKX_CPUS"))
    unlockBinding("detectCores", ns)
    assign("detectCores", function(all.tests = FALSE, logical = TRUE) min(cap, original(all.tests, logical)), envir = ns)
    lockBinding("detectCores", ns)
  })
}
RPROFILE
