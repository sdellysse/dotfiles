set -q fisher_path; or set -g fisher_path "$XDG_CACHE_HOME/fisher_workdir"
if not [ -d "$fisher_path" ]
		mkdir -p "$fisher_path"
end

# Keep fisher config inside this dir; fisher expects it in $fisher_path
if not [ -L "$fisher_path/fishfile" ]
	ln -sf "$XDG_CONFIG_HOME/fish/fisher" "$fisher_path/fishfile"
end

# Download fisher into its working directory if needed
if not [ -e "$fisher_path/fish/actual_fisher.fish" ]
		curl https://git.io/fisher --create-dirs -sLo "$fisher_path/actual_fisher.fish"
end
builtin source "$fisher_path/actual_fisher.fish"


# add the fisher workdir to the paths and pull in fisher configs
set fish_function_path $fish_function_path[1] $fisher_path/functions $fish_function_path[2..-1]
set fish_complete_path $fish_complete_path[1] $fisher_path/completions $fish_complete_path[2..-1]
for file in $fisher_path/conf.d/*.fish
		builtin source $file 2> /dev/null
end
