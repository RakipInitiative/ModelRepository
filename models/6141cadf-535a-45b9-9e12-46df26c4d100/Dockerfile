# Outbreak simulation on the shared Python base (bases/python-3.12). Legacy: FROM python:3.9 + libgdal-dev, unpinned pip (built 2025-03-10).
# geopandas 1.0.1 was the last release for Python 3.9, the newest the legacy build could resolve. Its wheels (pyogrio,
# pyproj, shapely) carry GDAL and PROJ, so no libgdal-dev or gcc is needed. The output is a file (outbreakFile).
FROM harbor.cloudferro-dev.risk-ai-cloud.com/fskx-base/python-3.12@sha256:d17a7d550130c2e19a40235282be689fb70ed7d22fadc31583092b3335a630b6

RUN /usr/local/bin/pip install --no-cache-dir ipfn==1.4.4 scipy==1.15.2 \
 geopandas==1.0.1 shapely==2.0.7 pyogrio==0.10.0 pyproj==3.7.1 \
 && /usr/local/bin/python -c "import ipfn, scipy, geopandas, shapely, pyogrio, pyproj"

ADD . /containerscripts
ADD fskx /fskx
ENTRYPOINT . ./containerscripts/entrypoint.sh
