#!/bin/sh
# Install the official Bitcoin Core and Bitcoin Cash Node release binaries
# into /opt/bitcoin-core and /opt/bchn, after checking their SHA-256 hashes.
#
# The Containerfile runs this while building the regtest image. It reads the
# versions and hashes from the build arguments declared there:
# CORE_VERSION, BCHN_VERSION, and CORE_SHA256_<arch> and BCHN_SHA256_<arch>
# for each supported architecture.
set -eu

# Podman sets TARGETARCH; fall back to the architecture of the build host.
arch="${TARGETARCH:-$(dpkg --print-architecture)}"

case "$arch" in
  amd64)
    triple=x86_64-linux-gnu
    core_sha="$CORE_SHA256_amd64"
    bchn_sha="$BCHN_SHA256_amd64"
    ;;
  arm64)
    triple=aarch64-linux-gnu
    core_sha="$CORE_SHA256_arm64"
    bchn_sha="$BCHN_SHA256_arm64"
    ;;
  *)
    echo "unsupported architecture: $arch" >&2
    exit 1
    ;;
esac

# Download a release tarball, check its hash and unpack it into a directory.
install_release() {
  url="$1"
  sha="$2"
  dest="$3"
  file="${url##*/}"
  curl -fsSLO "$url"
  echo "${sha}  ${file}" | sha256sum -c -
  mkdir -p "$dest"
  tar -xzf "$file" -C "$dest" --strip-components=1
  rm -f "$file"
}

cd /tmp

core_base="https://bitcoincore.org/bin/bitcoin-core-${CORE_VERSION}"
install_release \
  "${core_base}/bitcoin-${CORE_VERSION}-${triple}.tar.gz" \
  "$core_sha" \
  /opt/bitcoin-core

bchn_base="https://github.com/bitcoin-cash-node/bitcoin-cash-node"
bchn_base="${bchn_base}/releases/download/v${BCHN_VERSION}"
install_release \
  "${bchn_base}/bitcoin-cash-node-${BCHN_VERSION}-${triple}.tar.gz" \
  "$bchn_sha" \
  /opt/bchn

# Show the installed versions in the build log.
/opt/bitcoin-core/bin/bitcoind -version | head -1
/opt/bchn/bin/bitcoind -version | head -1
