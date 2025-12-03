# This currently does nothing as fish does not have completions
# for nix-* commands.
complete --command nom-build --wraps nix-build
complete --command nom-shell --wraps nix-shell

function __fish_complete_nom_like_nix
    # Remove the first word from the commandline because that is "nom"
    set --local cmdline (commandline -xpc | string escape; commandline -ct)[2..-1]
    complete --do-complete "nix $cmdline"
end

complete --command nom --condition "__fish_seen_subcommand_from build develop shell copy" --arguments '(__fish_complete_nom_like_nix)'
