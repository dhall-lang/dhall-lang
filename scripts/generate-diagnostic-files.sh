set -eu

nix build --file ./release.nix expected-diagnostic-files
rsync --archive --checksum --delete result/ ./tests
chmod -R u+w ./tests
