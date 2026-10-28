"""Generates the Dockerfile for an FSKX model.

Ported from fskx-container-image-management-service/generate_dockerfile.py.
Behaviour is unchanged, except that every FROM line is pinned to linux/amd64
so a build on an Apple-silicon machine cannot silently produce an arm64 image.
"""
import logging

from FskxFile import FskxFile, ProgrammingLanguage

logger = logging.getLogger(__name__)

PLATFORM = "linux/amd64"


class DockerHubBase():
    python = "python"
    r = "rocker/r-ver"
    r_tidyverse = "rocker/tidyverse"


def build_r_dependencies_install_commands(dependencies):
    install_list = []
    # system libraries needed to compile R packages (the configure step fails without them)
    if "gsl" in dependencies.keys():
        install_list.append("RUN apt-get update && apt-get install -y libgsl-dev\n")
    install_list.append("RUN R -e \"install.packages('remotes')\"")
    install_list.append(f"\nRUN R -e \"install.packages('jsonlite')\"")
    for dependency, version in dependencies.items():

        # Why?
        if dependency == "tidyverse":
            continue

        if version:
            install_list.append(f"\nRUN R -e \"require(remotes)\" -e \"install_version('{dependency}', version = '{version}')\"")
        else:
            install_list.append(f"\nRUN R -e \"install.packages('{dependency}')\"")
    logger.debug(f"Commands for installing packages: {install_list}")
    return install_list


def build_python_dependencies_install_commands(dependencies):
    install_list = []

    # handle special cases
    if "geopandas" in dependencies.keys():
        install_list.append("\nRUN apt-get update && apt-get install -y libgdal-dev gcc")
    if "pandas" not in dependencies.keys():
        # needed for exporting variables
        install_list.append("\nRUN /usr/local/bin/pip install pandas")

    for dependency, version in dependencies.items():
        if version:
            install_list.append(f"\nRUN /usr/local/bin/pip install \"{dependency}=={version}\"")
        else:
            install_list.append(f"\nRUN /usr/local/bin/pip install {dependency}")
    logger.debug(f"Commands for installing packages: {install_list}")
    return install_list


def dockerfile_from_model(model: FskxFile):
    if model.programming_language == ProgrammingLanguage.python:
        packages_lst = build_python_dependencies_install_commands(model.dependencies)
        dockerhub_base_image = DockerHubBase.python
        # FSKX files declare "Python 3.4.8", which the old generator mapped to python:3.5.
        # That image is Debian buster (EOL, apt mirrors gone), so it cannot be built any more.
        # Interim: python:3.11. TODO: check per model whether the code still runs.
        if model.programming_version == "3.4.8":
            dockerhub_base_image_version = "3.11"
        else:
            dockerhub_base_image_version = model.programming_version

    elif model.programming_language == ProgrammingLanguage.r:
        packages_lst = build_r_dependencies_install_commands(model.dependencies)
        # `R -e install...` exits 0 even when a package fails to install, so check explicitly.
        wanted = ["remotes", "jsonlite"] + [d for d in model.dependencies if d != "tidyverse"]
        wanted_r = ", ".join(f"'{p}'" for p in wanted)
        packages_lst.append(
            "\nRUN R -e \"missing <- setdiff(c(" + wanted_r + "), rownames(installed.packages())); "
            "if (length(missing) > 0) stop(paste('R packages failed to install:', paste(missing, collapse = ', ')))\"")
        if "tidyverse" in model.dependencies.keys():
            dockerhub_base_image = DockerHubBase.r_tidyverse
        dockerhub_base_image_version = model.programming_version
        if model.programming_version == "3":
            dockerhub_base_image = DockerHubBase.r_tidyverse
            dockerhub_base_image_version = "3.6.3-ubuntu18.04"
        else:
            dockerhub_base_image = DockerHubBase.r

    dockerfile_content = f"""
FROM --platform={PLATFORM} {dockerhub_base_image}:{dockerhub_base_image_version}

RUN apt-get update && apt-get install -y curl
RUN apt install cmake -y
RUN echo "" > .Rprofile
RUN echo "options(repos = c(CRAN = 'http://cloud.r-project.org', RForge = 'http://R-Forge.R-project.org'))" >> .Rprofile

ADD . /containerscripts
ADD fskx /fskx
# Download the latest Miniconda installer
RUN curl -sSL https://repo.anaconda.com/miniconda/Miniconda3-py38_23.5.0-3-Linux-x86_64.sh -o /miniconda.sh
RUN bash /miniconda.sh -b -p $HOME/miniconda
ENV PATH="/root/miniconda/bin:${{PATH}}"
RUN conda init
RUN conda env create -f /containerscripts/environment.yml
{' '.join(packages_lst)}
ENTRYPOINT . ./containerscripts/entrypoint.sh
"""
    return dockerfile_content
