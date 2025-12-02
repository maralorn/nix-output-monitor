complete --command nom-build --wraps nix-build
complete --command nom-shell --wraps nix-shell

complete --command nom --condition "__fish_seen_subcommand_from build" --wraps "nix build"
complete --command nom --condition "__fish_seen_subcommand_from develop" --wraps "nix develop"
complete --command nom --condition "__fish_seen_subcommand_from shell" --wraps "nix shell"
complete --command nom --condition "__fish_seen_subcommand_from copy" --wraps "nix copy"
