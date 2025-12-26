#compdef nom

# File copied from github.com/nixos/nix/misc/zsh/completion.zsh and adapted for nom

function _nom() {
  local ifs_bk="$IFS"
  local input=("${(Q)words[@]}")
  IFS=$'\n'
  local res=($(NIX_GET_COMPLETIONS=$((CURRENT - 1)) "$input[@]" 2>/dev/null))
  IFS="$ifs_bk"
  local tpe="${${res[1]}%%>	*}"
  local -a suggestions
  declare -a suggestions
  for suggestion in ${res:1}; do
    suggestions+=("${suggestion%%	*}")
  done
  local -a args
  if [[ "$tpe" == filenames ]]; then
    args+=('-f')
  elif [[ "$tpe" == attrs ]]; then
    args+=('-S' '')
  fi
  compadd -J nom "${args[@]}" -a suggestions
}

_nom "$@"
