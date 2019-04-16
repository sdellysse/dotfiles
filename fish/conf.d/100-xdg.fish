[ "$XDG_DATA_HOME"   != "" ] || set --global --export XDG_DATA_HOME "$HOME/.local/share"
[ "$XDG_CONFIG_HOME" != "" ] || set --global --export XDG_CONFIG_HOME "$HOME/.config"
[ "$XDG_CACHE_HOME"  != "" ] || set --global --export XDG_CACHE_HOME "$HOME/.cache"
