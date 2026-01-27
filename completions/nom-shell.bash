__load_completion nix-shell
_nom_shell_completion() {
    COMP_WORDS[0]=nix-shell
    COMP_LINE="nix-shell${COMP_LINE#nom-shell}"
    _nix_completion
}
complete -F _nom_shell_completion nom-shell
