# WSL doesn't set a sane umask, fix that here
if [ "$( umask )" = "0000" ]; then
  umask 0002
fi

# Make sure XDG is set up correctly
[ "$XDG_CONFIG_HOME" != "" ] || export XDG_CONFIG_HOME="${HOME}/config"
[ "$XDG_DATA_HOME"   != "" ] || export XDG_DATA_HOME="${HOME}/.local/share"
[ "$XDG_CACHE_HOME"  != "" ] || export XDG_CACHE_HOME="${HOME}/.cache"

export HISTFILE="${XDG_CACHE_DIR}/bash_history"


# auto-install nvm
if [ ! -e "${HOME}/.nvm/nvm.sh" ]; then
	echo "Install nvm..."
	git clone https://github.com/creationix/nvm "${HOME}/.nvm"
	pushd "${HOME}/.nvm"
	git checkout $( git describe --abbrev=0 --tags --match "v[0-9]*" $( git rev-list --tags --max-count=1 ) )
	popd
fi

# Defer initialization of nvm until nvm, node or a node-dependent command is
# run. Ensure this block is only run once if .bashrc gets sourced multiple times
# by checking whether __init_nvm is a function.
# https://www.growingwiththeweb.com/2018/01/slow-nvm-init.html
if [ -s "${HOME}/.nvm/nvm.sh" ] && [ ! "$(type -t __init_nvm)" = function ]; then
  export NVM_DIR="${HOME}/.nvm"

  declare -a __node_commands=('nvm' 'node' 'npm' 'yarn' 'gulp' 'grunt' 'webpack')

  function __init_nvm() {
    for i in "${__node_commands[@]}"; do
      unalias $i
    done

    source  "${NVM_DIR}/nvm.sh"
    unset __node_commands
    unset -f __init_nvm
  }

  for i in "${__node_commands[@]}"; do
    alias $i='__init_nvm && '$i
  done
fi
