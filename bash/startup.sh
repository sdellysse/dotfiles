# ~/.config/dotfiles/bash/startup.sh

# ensure sane umask
	umask 0002

# Set variables for both the noninteractive and (inherited) interactive
# sessions
	[ "$XDG_CONFIG_HOME" = "" ] && export XDG_CONFIG_HOME="${HOME}/.config"
	[ "$XDG_DATA_HOME"   = "" ] && export XDG_DATA_HOME="${HOME}/.local/share"
	[ "$XDG_CACHE_HOME"  = "" ] && export XDG_CACHE_HOME="${HOME}/.cache"
		# Make sure XDG is set up correctly, but don't clobber existing values

	[ "$BASH_ENV" = "" ] && export BASH_ENV="${HOME}/.config/dotfiles/bash/startup.sh"
		# Make sure we set where bash should look for its non-interactive env
		# settings

# vim: ft=sh
