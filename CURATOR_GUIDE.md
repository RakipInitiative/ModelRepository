# Curator guide: from a new FSKX model to a runnable container image

For curators who prepare the execution image of a model. You do not need to know Kubernetes. You need a GitHub account
with access to this repository, Docker Desktop, and Python 3.10 or newer.

> **Status (2026-10-30, draft for review).** Steps 1-3 and 5-9 describe what works today. Step 4 (choosing a shared base
> image) describes the **planned** way of writing the Dockerfile. The base images ([bases/](bases/)) and the workflow that
> builds them ([build-base.yml](.github/workflows/build-base.yml)) exist in the repository as a proposal, but nothing has
> been published to Harbor yet. Until a maintainer announces that the bases are available, start from the full Dockerfile
> that `init` generates (step 3) and skip step 4.

## What happens to your model

```
you: upload FSKX file to the platform -> model id
you: branch + folder models/<model_id>/ (Dockerfile + build.yaml) -> test locally -> pull request
GitHub Action: builds the image, runs it once (smoke test)            (pull request: nothing is published)
maintainer: reviews, merges
GitHub Action: builds again and pushes  harbor.../fskx/models/<model_id>:<version>
platform: finds the new image within about 15 minutes and makes it runnable
```

A model folder holds exactly two files, `Dockerfile` and `build.yaml`. The FSKX file itself is **not** stored in git: the
Action downloads it from the platform when it builds.

## Step 1 - Upload the model to the platform

Upload the FSKX file in the platform so that it has a **model id** (a UUID like `9731510d-5dc3-4da6-9c2c-d5544b253489`).
The model id is the name of your folder in step 3. The model can still be uncurated; the Action reads it with the
platform's API key.

*The main script named in the model's metadata (`metadata.rdf`) must be an `.r` or `.py` file; other languages are not
supported by the execution harness.*

## Step 2 - Get the repository and make a branch

The full repository is large (about 1.4 GB because of old content). Clone only what you need:

```bash
git clone --filter=blob:none --sparse https://github.com/RakipInitiative/ModelRepository.git
cd ModelRepository
git sparse-checkout set tools harness bases models/<model_id>
git checkout -b model-<model_id>
```

You need an API key to download the model file for local tests. Ask a platform administrator for one. **Never put it
into a file, a commit, a pull request or a chat message**; keep it in your shell only:

```bash
export FSKX_API_KEY='<your key>'
```

## Step 3 - Create the model folder

```bash
python3 tools/prepare_model.py init <model_id> --models-dir models
```

This downloads the model, creates `models/<model_id>/Dockerfile` and `build.yaml`. The generated Dockerfile is only a
starting point from the packages named in the model; it often needs manual fixes. If you already have a Dockerfile that
works, use `--dockerfile my.Dockerfile` instead. Whatever you commit is built exactly as it is.

## Step 4 - Choose a base image (planned; skip until announced)

Pick the family that matches your model and start your Dockerfile with `FROM` that base. The base already contains the
operating system, the language runtime, Miniconda, the harness environment and the common packages, so your Dockerfile
only adds what is specific to your model.

| Your model needs | Base | Already in it |
|---|---|---|
| R 3.6.3 (tidyverse) | `bases/r-3.6.3` | rocker/tidyverse 3.6.3, curl, cmake, `remotes`, `jsonlite` |
| Python 3.12 | `bases/python-3.12` | pandas 2.2.3, numpy 2.2.4, matplotlib 3.10.1, openpyxl 3.1.5, seaborn 0.13.2 |
| another R or Python version, system libraries (e.g. GDAL), Octave | none yet | write a full Dockerfile as in step 3 and ask a maintainer whether a new base is worthwhile |

`FROM` must name the base **by digest** (`FROM <registry>/fskx/base/<name>@sha256:<digest>`); the exact line is
published together with the base. The examples show the shape: [bases/r-3.6.3/example-9731510d.Dockerfile](bases/r-3.6.3/example-9731510d.Dockerfile)
and [bases/python-3.12/example-7c962507.Dockerfile](bases/python-3.12/example-7c962507.Dockerfile).

Why it matters: a node stores a layer once only if it is identical in all images that use it. With a shared base the
model above added 9 MB (R) and 46 MB (Python) on top of the base instead of roughly 800-900 MB, and starts in seconds when the
base is already on the machine.

## Step 5 - Write the Dockerfile

Rules that keep images small and results reproducible:

1. **Pin every version.** `pip install pandas` or `install.packages("x")` take the newest release at build time. A model
   written for pandas 2 failed on pandas 3 in our test. Use `pandas==2.2.3`, `install_version('ggplot2', version = '3.3.0')`
   and a dated base (`rocker/tidyverse:3.6.3-ubuntu18.04`, not `latest`).
   **For R also pin the CRAN date.** `install_version` pins the package you name, but its *dependencies* come from
   whatever the repository offers today, and old R versions fall off the end of CRAN: on R 3.6.3, `ggplot2 3.5.1`
   no longer installs from current CRAN (its dependency `cpp11` needs R 4.0 since December 2024). Point R to a dated
   snapshot from the time the model was written, before the install steps:
   ```dockerfile
   RUN echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/2024-11-01'))" > /.Rprofile
   ```
   (R reads `/.Rprofile` because the build runs R from `/`.) Example: [bases/r-3.6.3/example-ggplot2-group.Dockerfile](bases/r-3.6.3/example-ggplot2-group.Dockerfile).
2. **Keep the model-specific part last.** The harness scripts and the model go at the very end, exactly like this:
   ```dockerfile
   ADD . /containerscripts
   ADD fskx /fskx
   ENTRYPOINT . ./containerscripts/entrypoint.sh
   ```
   Anything above these lines can be shared with other models, anything below cannot.
3. **Do not touch the harness** (`harness/`) in a model pull request. The harness is shared by all models.
4. **Check that installs worked.** R: end with a `RUN R -e "missing <- setdiff(c('pkg1','pkg2'), rownames(installed.packages())); if (length(missing) > 0) stop(...)"` step, as in `models/9731510d-.../Dockerfile`. A package that fails to install otherwise only shows up when the model runs.
5. **Clean up in the same `RUN`** that creates the mess (`&& rm /installer.sh`, `pip install --no-cache-dir`). A later `RUN rm` does not shrink the image.
6. **Platform:** the Action builds `linux/amd64`. Do not select another platform in `FROM`.

## Step 6 - Fill in `build.yaml`

```yaml
version: 1     # positive integer: 1, 2, 3, ...  (not 0, 1.5, 01, v2)
cpu: 1         # CPU cores the simulation needs
ram: 1         # GB of memory
gpu: 0
```

`version` becomes the image tag. The platform runs the **highest** version of a model. A pull request fails if you change
a model folder without raising the version. A published version is never overwritten.

## Step 7 - Test locally (do this before the pull request)

```bash
python3 tools/prepare_model.py context models/<model_id> /tmp/ctx-<model_id> --fskx <path-to-model.fskx>
docker buildx build --platform linux/amd64 --load -t test/<model_id> -f models/<model_id>/Dockerfile /tmp/ctx-<model_id>
python3 tools/smoke_test.py test/<model_id> --timeout 1200 --keep /tmp/smoke-<model_id>
```

(`prepare_model.py fetch <model_id> model.fskx` downloads the file if you do not have it.) On a Mac with Apple Silicon
the amd64 build runs in emulation and is slower; a first build of an R image takes several minutes.

The smoke test runs the image the way the platform does, with the model's default parameters, and passes when
`/data/output.json` is written and not empty. Look at `/tmp/smoke-<model_id>/data` afterwards: plots, result files and
`simulation_log.log` (errors of your model are in the log, the container exit code says nothing).

*Known limit:* a model whose results are **only files** (for example a plot) writes `{}` to `output.json` and fails the
smoke test even though it works. If that is your case, say so in the pull request; a maintainer decides.

## Step 8 - Open the pull request

```bash
git add models/<model_id>
git commit -m "Add execution image for model <model_id> (<short model name>)"
git push -u origin model-<model_id>
```

Open a pull request against `master`. The Action starts a build and smoke test of your model. A maintainer has to
**approve the run** first (it needs the platform's API key), so the check may show "waiting" for a while. A pull request
never publishes anything.

Checks that can fail: invalid or not raised `version` (step 6), build error (read the log of the failed step), smoke test.

## Step 9 - Merge and publish

After review a maintainer merges the pull request. The merge starts the Action again: build, smoke test and **push to
Harbor** as `fskx/models/<model_id>:<version>` with labels (model id, version, cpu, ram, harness version, hash of the model
file). Within about 15 minutes the platform's image management service finds the new image and makes the model runnable
at that version. If the model still cannot be started after half an hour, tell an administrator; they can trigger the
synchronisation and read its status.

## Updating an existing model

| What changed | What to do |
|---|---|
| The FSKX file (new model code) | Upload the new file to the platform **first**, then raise `version` in `build.yaml` (a Dockerfile change is not needed). The version bump is what makes the Action rebuild with the new file. |
| The Dockerfile (fix, new package) | Edit the Dockerfile and raise `version`. |
| Resources (cpu, ram, gpu) | Change `build.yaml` and raise `version`. |

If something goes wrong after publishing, raise the version again with the fix. Do not try to delete or overwrite a tag.

## Common problems

| Message or symptom | Cause and fix |
|---|---|
| "version must be a positive integer" / "not higher" | Step 6. Use whole numbers and a number higher than the one on `master`. |
| Build cannot download the model | The model id in the folder name is wrong, or the Action has no permission for an uncurated model: ask a maintainer to check the run approval and the API key. |
| `apt-get update` fails in an old image (Ubuntu 18.04, Debian buster, python:3.5) | These distributions' repositories moved to archive servers. Point `sources.list` to the archive or use a newer base, and test the model again. |
| An R package version "is not available" or does not compile | Old versions are in the CRAN archive (`install_version`), but they may need system libraries (`apt-get install -y libgsl-dev` for `gsl`). Model `9731510d` shows both. |
| `package 'xyz' is not available (for R version 3.6.3)` | A dependency of your pinned package needs a newer R than today's CRAN offers for yours. Use a dated CRAN snapshot (step 5, rule 1) from before that package moved on, or pin that dependency to an older version first (`install_version('xyz', version = '...')`). |
| A package that used to be on CRAN cannot be found at all | It was removed (example: `TruncatedDistributions`). Take the installed package from an existing image of the model (`COPY --from=<old image> <library path>/<package> ...`) or ask the author for the source; tell a maintainer. |
| Model works with the old image but fails now with a type or attribute error | Unpinned package versions (step 5, rule 1). Pin to the versions the model was written for. |
| `output.json is empty` in the smoke test | Step 7, known limit for file-only results. |
| "no space left on device" during the build | The GitHub runner has about 14 GB. Install fewer or smaller packages, merge `RUN` steps that leave large temporary files, use a shared base. |

## Please do not

* commit FSKX files, keys, passwords or tokens;
* change `harness/`, `tools/` or the workflow in a model pull request;
* push images or tags to Harbor by hand;
* use `latest` tags or unpinned versions.
