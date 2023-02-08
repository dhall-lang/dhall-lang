set -eu

nix --extra-experimental-features nix-command build --file ./release.nix expected-test-files
rsync --archive --checksum --delete result/ ./tests
chmod -R u+w ./tests
