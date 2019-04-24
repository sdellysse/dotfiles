# WSL doesn't always set a sane umask, fix that here
umask 0002

# Make sure XDG is set up correctly
[ "$XDG_CONFIG_HOME" != "" ] || export XDG_CONFIG_HOME="${HOME}/.config"
[ "$XDG_DATA_HOME"   != "" ] || export XDG_DATA_HOME="${HOME}/.local/share"
[ "$XDG_CACHE_HOME"  != "" ] || export XDG_CACHE_HOME="${HOME}/.cache"

export HISTFILE="${XDG_CACHE_DIR}/histfile"
export BASH_ENV="${XDG_CONFIG_HOME}/dotfiles/bash_noninteractive.sh"
