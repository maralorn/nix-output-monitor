__load_completion nix-build
_nom_build_completion() {
    COMP_WORDS[0]=nix-build
    COMP_LINE="nix-build${COMP_LINE#nom-build}"
    _nix_completion
}
complete -F _nom_build_completion nom-build
