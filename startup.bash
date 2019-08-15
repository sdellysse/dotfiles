# ensure sane umask
	umask 0002

# Make sure XDG is set up correctly
	[ "$XDG_CONFIG_HOME" = "" ] && export XDG_CONFIG_HOME="${HOME}/.config"
	[ "$XDG_DATA_HOME"   = "" ] && export XDG_DATA_HOME="${HOME}/.local/share"
	[ "$XDG_CACHE_HOME"  = "" ] && export XDG_CACHE_HOME="${HOME}/.cache"

# Keep your $HOME clean
	export HISTFILE="${HOME}/.cache/histfile"

# Make sure we set where bash should look for its non-interactive env settings
  export BASH_ENV="${HOME}/.config/dotfiles/startup.bash"

# Launch fish if needed
	( [ -x /usr/sbin/fish ] && [ "$BASH_TO_FISHED" != "done" ] && [[ $- == *i* ]] && shopt -q login_shell ) && export BASH_TO_FISHED="done" && exec /usr/sbin/fish --interactive --login
	( [ -x /usr/sbin/fish ] && [ "$BASH_TO_FISHED" != "done" ] && [[ $- == *i* ]] )                         && export BASH_TO_FISHED="done" && exec /usr/sbin/fish --interactive
	[ -x /usr/sbin/fish ] || echo "INSTALL FISH"
