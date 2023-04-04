set -eu

my_nix() {
    nix --extra-experimental-features 'nix-command flakes' "${@}"
}

my_rsync() {
    my_nix run 'nixpkgs#rsync' -- "${@}"
}

my_nix build --file ./release.nix expected-prelude
my_rsync --archive --checksum --delete result/ ./Prelude
chmod -R u+w ./Prelude
