#!/usr/bin/env bash
# Fail if stack.yaml refers to a package by a local path outside the
# repository, such as an extra-dep pointing at ../haskoin-core.
#
# CI checks out only this repository, so such a dependency cannot build
# there. Pin a released version, or a git commit, in extra-deps instead.
set -euo pipefail

# A list item whose value starts with ./, ../, / or ~ is a local path.
pattern='^[[:space:]]*-[[:space:]]*(\.\.?/|/|~)'

if grep -nE "$pattern" stack.yaml; then
  message="stack.yaml refers to local paths outside the repository."
  message+=" Pin released versions (or a git commit) in extra-deps"
  message+=" before pushing."
  # GitHub Actions shows this as an error annotation on stack.yaml.
  echo "::error file=stack.yaml::$message"
  exit 1
fi
