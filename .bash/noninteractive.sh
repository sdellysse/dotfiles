export LANG="en_US.utf-8"
export PATH="${HOME}/opt/bin:${HOME}/bin:$PATH"
export EDITOR="vim"
export HISTFILE="${HOME}/tmp/bash_history"

# Defer initialization of nvm until nvm, node or a node-dependent command is
# run. Ensure this block is only run once if .bashrc gets sourced multiple times
# by checking whether __init_nvm is a function.
# https://www.growingwiththeweb.com/2018/01/slow-nvm-init.html
if [ -s "$HOME/.nvm/nvm.sh" ] && [ ! "$(type -t __init_nvm)" = function ]; then
  export NVM_DIR="$HOME/.nvm"
  
  [ -s "$NVM_DIR/bash_completion" ] && source "$NVM_DIR/bash_completion"

  declare -a __node_commands=('nvm' 'node' 'npm' 'yarn' 'gulp' 'grunt' 'webpack')

  function __init_nvm() {
    for i in "${__node_commands[@]}"; do
      unalias $i
    done

    source  "$NVM_DIR"/nvm.sh
    unset __node_commands
    unset -f __init_nvm
  }

  for i in "${__node_commands[@]}"; do
    alias $i='__init_nvm && '$i
  done
fi
