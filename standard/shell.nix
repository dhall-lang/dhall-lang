# Entering this shell runs ./link-literate.sh so `cabal build` finds the
# Module.lhs symlinks to the kebab-case literate Markdown sources.
(import ../release.nix {}).standard.env.overrideAttrs (old: {
  shellHook = (old.shellHook or "") + ''
    bash "${toString ./.}/link-literate.sh"
  '';
})
