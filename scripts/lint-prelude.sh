#!/usr/bin/bash

set -eu

my_nix() {
    nix --extra-experimental-features 'nix-command flakes' "${@}"
}

my_rsync() {
    my_nix run 'nixpkgs#rsync' -- "${@}"
}

if command -v nix > /dev/null; then
    my_nix build --file ./release.nix expected-prelude
    my_rsync --archive --checksum --delete result/ ./Prelude
    chmod -R u+w ./Prelude
else
    files=( $(find Prelude -type f ! -name 'README.md') )
    dhall lint "${files[@]}"
    dhall freeze --all --cache "${files[@]}"
fi
