#!/usr/bin/env bash
# Create (or refresh) GHC-facing .lhs symlinks to the kebab-case literate
# Markdown sources. After A00 the .md files are the only committed literate
# sources; markdown-unlit still requires a Module.lhs path at compile time.
set -euo pipefail

cd "$(dirname "$0")"

link() {
    local target="$1"
    local source="$2"

    if [[ ! -e "$source" ]]; then
        echo "link-literate.sh: missing source $source" >&2
        exit 1
    fi

    if [[ -L "$target" ]]; then
        ln -sfn "$source" "$target"
    elif [[ -e "$target" ]]; then
        rm -f "$target"
        ln -sfn "$source" "$target"
    else
        ln -sfn "$source" "$target"
    fi
}

link Syntax.lhs syntax.md
link AlphaNormalization.lhs alpha-normalization.md
link BetaNormalization.lhs beta-normalization.md
link Binary.lhs binary.md
link Equivalence.lhs equivalence.md
link FunctionCheck.lhs function-check.md
link Multiline.lhs multiline.md
link Shift.lhs shift.md
link Substitution.lhs substitution.md
link TypeInference.lhs type-inference.md
