# ~/.config/dotfiles/fish/config.fish

# ensure sane umask
	umask 0002

# Ensure XDG is set up
	set --query $XDG_CONFIG_HOME && set --global --export XDG_CONFIG_HOME $HOME/.config
	set --query $XDG_DATA_HOME   && set --global --export XDG_DATA_HOME   $HOME/.local/share
	set --query $XDG_CACHE_HOME  && set --global --export XDG_CACHE_HOME  $HOME/.cache

# suppress greeting
	set fish_greeting

# Make sure we set where bash should look for its non-interactive env settings
	set --global --export BASH_ENV $HOME/.config/dotfiles/bash/startup.sh

# Keep your $HOME clean
	set --global --export HISTFILE $HOME/.cache/histfile

# setup interactive envvars here
	set --global --export ASDF_CONFIG_FILE $XDG_CONFIG_HOME/asdfrc
	set --global --export ASDF_DATA_DIR    $XDG_CACHE_HOME/asdf
	set --export --global EDITOR           "/usr/local/bin/code --wait"
	set --export --global LESSHISTFILE     $XDG_CACHE_DIR/lesshistfile