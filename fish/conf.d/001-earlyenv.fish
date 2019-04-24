# WSL doesn't always set a sane umask, fix that here
umask 0002

# Make sure XDG is set
set --query XDG_DATA_HOME   || set --global --export XDG_DATA_HOME   $HOME/.local/share
set --query XDG_CONFIG_HOME || set --global --export XDG_CONFIG_HOME $HOME/.config
set --query XDG_CACHE_HOME  || set --global --export XDG_CACHE_HOME  $HOME/.cache

# Make sure non-interactive bash has the right config
set --global --export BASH_ENV $XDG_CONFIG_HOME/dotfiles/bash_noninteractive.sh
