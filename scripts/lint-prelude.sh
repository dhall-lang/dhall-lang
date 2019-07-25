set -eu

nix build --file ./release.nix expected-prelude
rsync --archive --checksum --delete result/ ./Prelude
chmod -R u+w ./Prelude
