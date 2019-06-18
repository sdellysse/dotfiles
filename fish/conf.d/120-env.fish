set --export --global DOCKER_HOST  "tcp://localhost:2375"
set --export --global EDITOR       "nvim"
set --export --global LESSHISTFILE "$XDG_CACHE_DIR/lesshistfile"
set --export --global LS_BINPATH   "/usr/local/bin/wsl_ls"

eval ( ~/gdrive/software/weasel-pageant-1.1/weasel-pageant -rq -S fish )
