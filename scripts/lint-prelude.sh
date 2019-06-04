set -eu

nix build --file ./release.nix expected-prelude
rsync --archive --delete result/ ./Prelude
chmod --recursive u+w ./Prelude
