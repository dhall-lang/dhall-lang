set -eu

my-nix() {
    nix --extra-experimental-features 'nix-command flakes' "${@}"
}

my-rsync() {
    my-nix run 'nixpkgs#rsync' -- "${@}"
}

my-nix build --file ./release.nix expected-test-files
my-rsync --archive --checksum --delete result/ ./tests
chmod -R u+w ./tests
