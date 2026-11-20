#!/usr/bin/env bash
# Install the C libraries that haskoin-node links against on Ubuntu, for a
# build without Nix. The regtest job of CI runs it; the shared workflow of
# jprupp/tools installs the same packages for the other jobs.
#
# Any arguments name extra packages to install along with them, for example
# podman for the regtest tests (Haskoin.Node.RegtestSpec).
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
