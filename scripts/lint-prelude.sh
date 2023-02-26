set -eu

my-nix() {
    nix --extra-experimental-features 'nix-command flakes' "${@}"
}

my-rsync() {
    my-nix run 'nixpkgs#rsync' -- "${@}"
}

my-nix build --file ./release.nix expected-prelude
my-rsync --archive --checksum --delete result/ ./Prelude
chmod -R u+w ./Prelude
