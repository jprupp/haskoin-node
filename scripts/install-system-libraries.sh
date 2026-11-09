#!/usr/bin/env bash
# Install the C libraries that haskoin-node links against on Ubuntu, as the
# CI jobs that build without Nix need them.
#
# Any arguments name extra packages to install along with them, for example
# podman for the regtest suite.
set -euo pipefail

packages=(
  librocksdb-dev
  libsecp256k1-dev
  pkg-config
  zlib1g-dev
  "$@"
)

sudo apt-get update
sudo apt-get install -y "${packages[@]}"
