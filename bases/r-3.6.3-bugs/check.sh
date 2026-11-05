#!/bin/bash
# Runs INSIDE the built base image. Fails on the first problem.
set -e
. /root/miniconda/etc/profile.d/conda.sh && conda activate container-env
python -c "import boto3, requests, libsbml; print('harness env ok')"
R -q -e "library(remotes); library(jsonlite); library(ggplot2); stopifnot(getRversion() == '3.6.3'); cat('R', as.character(getRversion()), 'ok\n')"
# OpenBUGS: the binary exists and R2OpenBUGS (both versions the models declare, from the base's dated CRAN) runs a small model through it
test -x /usr/local/bin/OpenBUGS
cd /tmp
for v in 3.2-3.2 3.2-3.2.1; do
  Rscript -e "remotes::install_version('R2OpenBUGS', version = '$v', upgrade = 'never'); stopifnot(installed.packages()['R2OpenBUGS', 'Version'] == '$v')"
done
cat > /tmp/model.txt <<'BUGS'
model { for (i in 1:N) { y[i] ~ dnorm(mu, 1) }  mu ~ dnorm(0, 0.0001) }
BUGS
Rscript -e "library(R2OpenBUGS); set.seed(1); y <- rnorm(30, 5, 1); fit <- bugs(list(y = y, N = 30), function() list(mu = 0), 'mu', model.file = '/tmp/model.txt', n.chains = 1, n.iter = 2000, n.burnin = 500, DIC = FALSE, working.directory = tempdir()); m <- fit\$mean\$mu; cat('posterior mean of mu:', m, '\n'); stopifnot(abs(m - mean(y)) < 0.3)"
echo "OpenBUGS ok"
