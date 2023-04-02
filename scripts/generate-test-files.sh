set -eu

my_nix() {
    nix --extra-experimental-features 'nix-command flakes' "${@}"
}

my_rsync() {
    my_nix run 'nixpkgs#rsync' -- "${@}"
}

my_nix build --file ./release.nix expected-test-files
my_rsync --archive --checksum --delete result/ ./tests
chmod -R u+w ./tests
