# Variant of the R 4.5 base for models that use OpenBUGS (R2OpenBUGS). Built FROM the plain r-4.5 base by digest, so a node that holds
# r-4.5 only pays for the extra layers here (the 32-bit runtime libraries and OpenBUGS, about 5 MB).
#
# OpenBUGS 3.2.3 is not packaged for Ubuntu. Every legacy model image downloaded the source from webbugs.psychstat.org and compiled it
# (leaving libc6-dev-i386, gcc-multilib and the build tree in the layers). Here it is compiled ONCE in a builder stage, the tarball is
# verified against the sha256 below (a changed or replaced upstream file fails the build) and only the binaries are copied. OpenBUGS on
# Linux is a 32-bit program: the builder needs gcc-multilib, the final image needs the 32-bit C runtime (libc6-i386).
#
# R2OpenBUGS itself (an R package) is installed by the model, in the version of its packages.json, like any other package.
# Build context = the harness/ directory (not used here, like the other bases).
ARG R_BASE=harbor.cloudferro-dev.risk-ai-cloud.com/fskx-base/r-4.5@sha256:7c4a3133bd4b4a65c7fd6f966682ac51b89188b8506d574299975f5e321c7c5f

FROM --platform=linux/amd64 ${R_BASE} AS build
ARG OPENBUGS_URL=https://webbugs.psychstat.org/wiki/Download/OpenBUGS-3.2.3.tar.gz
ARG OPENBUGS_SHA256=a289c4da0c4ac371f8013226c85ebd0a9e1dec5de9b858950e4854cb814b4ed8
RUN apt-get update && apt-get install -y --no-install-recommends build-essential gcc-multilib libc6-dev-i386 \
 && rm -rf /var/lib/apt/lists/*
RUN curl -fsSL -o /tmp/OpenBUGS.tar.gz "$OPENBUGS_URL" \
 && echo "$OPENBUGS_SHA256  /tmp/OpenBUGS.tar.gz" | sha256sum -c - \
 && tar -xzf /tmp/OpenBUGS.tar.gz -C /tmp \
 && cd /tmp/OpenBUGS-3.2.3 && ./configure && make && make install DESTDIR=/stage \
 && rm -rf /stage/usr/local/share

FROM --platform=linux/amd64 ${R_BASE}
RUN apt-get update && apt-get install -y --no-install-recommends libc6-i386 \
 && rm -rf /var/lib/apt/lists/*
COPY --from=build /stage/usr/local/ /usr/local/
RUN ldconfig
