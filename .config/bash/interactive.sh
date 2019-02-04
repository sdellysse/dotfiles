export BASH_ENV="${HOME}/.config/bash/noninteractive.sh"
source "$BASH_ENV"

eval $( ~/OneDrive/opt/weasel-pageant-1.1/weasel-pageant -rq )

#alias edit="emacs"
#alias editb="emacs -n"
alias ls="command wsl_ls --color --quoting-style=literal --group-directories-first --human-readable --indicator-style=slash"
alias l="ls -l"
alias la="l -a"
#alias s='screen -RD'
#alias -- -='cd -'
#alias vssh='vagrant ssh'

# http://boredzo.org/blog/archives/2016-08-15/colorized-man-pages-understood-and-customized
man() {
    env                            \
    LESS_TERMCAP_md=$'\e[1;36m'    \
    LESS_TERMCAP_me=$'\e[0m'       \
    LESS_TERMCAP_se=$'\e[0m'       \
    LESS_TERMCAP_so=$'\e[1;40;92m' \
    LESS_TERMCAP_ue=$'\e[0m'       \
    LESS_TERMCAP_us=$'\e[1;32m'    \
    man "$@"
}

shopt -s cdspell    # When changing directory small typos can be ignored by bash
shopt -s histappend # Make bash append rather than overwrite the history on disk
shopt -s cmdhist

if ((BASH_VERSINFO[0] >= 4)); then
    shopt -s autocd     # chdir just by typing dirname
    shopt -s dirspell   # fix spelling during tab completion
fi

PROMPT_COLOR_BLUE="\[\e[34;1m\]"
PROMPT_COLOR_BOLD="\[\e[1m\]"
PROMPT_COLOR_GREEN="\[\e[32;1m\]"
PROMPT_COLOR_RED="\[\e[1;31m\]"
PROMPT_COLOR_RESET="\[\e[m\]"
PROMPT_COLOR_YELLOW="\[\e[33;1m\]"
PROMPT_DIRECTORY_LENGTH_CUTOFF=16

function PROMPT_in_git_repo() {
    [ -d .git ] || git rev-parse --git-dir > /dev/null 2>&1
}

function PROMPT_not_home_repo() {
    [ "$( git rev-parse --show-toplevel 2>&1 )" != "$HOME" ]
}

function PROMPT_current_git_branch() {
    git rev-parse --abbrev-ref HEAD
}

function PROMPT_function() {
    local last_errno="$?"

    local exitstatus=
    if [ "${last_errno}" -eq 0 ]; then
      exitstatus="${PROMPT_COLOR_GREEN} :)"
    else
      exitstatus="${PROMPT_COLOR_RED}$( printf "%3s" "${last_errno}" )"
    fi

    local username=
    if [ "${UID}" -eq 0 ]; then
      username="${PROMPT_COLOR_RED}${USER}${PROMPT_COLOR_RESET}"
    else
      username="${PROMPT_COLOR_GREEN}${USER}${PROMPT_COLOR_RESET}"
    fi

    local dirs_str="$( dirs )"
    local pwd_str=
    if [ "${#dirs_str}" -gt "$PROMPT_DIRECTORY_LENGTH_CUTOFF" ]; then
        pwd_str="$(dirs | awk -F '/' -v ORS='/' '{
            for (i = 1; i < NF; i++) {
                print substr($i, 0, 1);
            }
            printf "%s", $NF;
        }')"
    else
        pwd_str="$dirs_str"
    fi

    local git_str=
    if PROMPT_in_git_repo && PROMPT_not_home_repo ; then
        git_str="${PROMPT_COLOR_GREEN}[$( PROMPT_current_git_branch )]${PROMPT_COLOR_RESET} "
    fi

    PS1="${username}@${PROMPT_COLOR_YELLOW}\h${PROMPT_COLOR_RESET} ${PROMPT_COLOR_BOLD}${exitstatus}${PROMPT_COLOR_RESET} ${PROMPT_COLOR_BLUE}${pwd_str}${PROMPT_COLOR_RESET} ${git_str}\$ "
    PS2="${PROMPT_COLOR_BOLD}>${PROMPT_COLOR_RESET} "
}
PROMPT_COMMAND=PROMPT_function

export MYSQL_PS1="[\\d]> "

which thefuck > /dev/null 2>&1
if [ "$?" == "0" ]; then
	eval $( thefuck --alias )
fi
