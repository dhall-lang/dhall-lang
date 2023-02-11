set -e

my-nix() {
    nix --extra-experimental-features 'nix-command flakes' "${@}"
}

my-nix build --file ./release.nix vm

trap 'rm --force dhall-lang.qcow2' EXIT

./result/bin/run-dhall-lang-vm
