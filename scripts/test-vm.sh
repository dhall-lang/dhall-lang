set -e

nix build --file ./release.nix vm

trap 'rm --force dhall-lang.qcow2' EXIT

./result/bin/run-dhall-lang-vm
