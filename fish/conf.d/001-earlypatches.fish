# WSL doesn't set a sane umask; set this as early as possible
if [ ( umask ) = "0000" ]
	umask 0002
end

# Make sure XDG is set
[ "$XDG_DATA_HOME"   != "" ] || set --global --export XDG_DATA_HOME "$HOME/.local/share"
[ "$XDG_CONFIG_HOME" != "" ] || set --global --export XDG_CONFIG_HOME "$HOME/.config"
[ "$XDG_CACHE_HOME"  != "" ] || set --global --export XDG_CACHE_HOME "$HOME/.cache"
