"""The rule for `version` in models/<model_id>/build.yaml, in one place for all tools.

A version is a positive integer written without leading zeros: 1, 2, 3, ... (not 0, -1, 1.5, 01, v2).

Why: the version becomes the image tag `fskx/models/<model_id>:<version>` in Harbor, and the
container image management service (CIMS, reconciler.py, VERSION_PATTERN) treats the image with
the highest integer tag as the active version of the model; a tag that is not such an integer is
REJECTED and never registered. The tools here refuse the same values earlier, so a mistake
fails in the pull request and not only after the merge. Keep both patterns identical.
"""
import re

VERSION_PATTERN = re.compile(r"^[1-9][0-9]*$")


def version_problem(value):
    """None if `value` is a valid version, otherwise a short explanation for the curator."""
    if VERSION_PATTERN.match(str(value)):
        return None
    return (f"version '{value}' is not valid: use a positive integer without leading zeros "
            "(1, 2, 3, ...); it becomes the image tag and the highest number is the active version")
