# WSL doesn't always set a sane umask, fix that here
	umask 0002

# Make sure XDG is set
	set --query XDG_DATA_HOME   || set --global --export XDG_DATA_HOME   $HOME/.local/share
	set --query XDG_CONFIG_HOME || set --global --export XDG_CONFIG_HOME $HOME/.config
	set --query XDG_CACHE_HOME  || set --global --export XDG_CACHE_HOME  $HOME/.cache

# Make sure non-interactive bash has the right config
	set --global --export BASH_ENV $XDG_CONFIG_HOME/dotfiles/startup.bash

# fisher setup
	# Tell fisher where to live
		set --global fisher_path $XDG_CACHE_HOME/fisher_workdir

	# Tell fish where to find fisher files
		set --global --export fish_function_path $XDG_CONFIG_HOME/dotfiles/fish/functions   $fisher_path/functions   $fish_function_path[2..-1]
		set --global --export fish_complete_path $XDG_CONFIG_HOME/dotfiles/fish/completions $fisher_path/completions $fish_complete_path[2..-1]

		builtin source $fisher_path/actual_fisher.fish

	# exec plugins conf.d files
		for file in $fisher_path/conf.d/*.fish
			builtin source $file 2> /dev/null
		end

# setup envvars because universal vars scare me
	set --export --global DOCKER_HOST  "tcp://localhost:2375"
	set --export --global EDITOR       "nvim"
	set --export --global LESSHISTFILE "$XDG_CACHE_DIR/lesshistfile"
	set --export --global LS_BINPATH   "/usr/local/bin/wsl_ls"
	set --export --global DISPLAY      :0

# connect to keeagent
	eval ( $ALLUSERSPROFILE/chocolatey/lib/weasel-pageant.portable/tools/weasel-pageant -rq -S fish )

# suppress greeting
	set fish_greeting
