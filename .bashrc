# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

OS=$(uname -o)
if [ $OS = 'Cygwin' ]; then
    HOSTNAME=$(hostname)
    export SSH_AUTH_SOCK=/tmp/.ssh-socket
    ssh-add -l >/dev/null 2>&1
    if [ $? = 2 ]; then
        eval $(ssh-agent -a $SSH_AUTH_SOCK)
        echo $SSH_AGENT_PID > /tmp/.ssh-agent-pid
    fi

    function kill-agent {
        kill $(cat /tmp/.ssh-agent-pid)
    }
else
    HOSTNAME=$(hostname -s)
fi

# When changing directory small typos can be ignored by bash
# for example, cd /vr/lgo/apaache would find /var/log/apache
shopt -s cdspell

[[ -f "${HOME}/.bash_aliases" ]] && source "${HOME}/.bash_aliases"
[[ -f /etc/bash_completion    ]] && source /etc/bash_completion

# Make bash append rather than overwrite the history on disk
shopt -s histappend

#####################################
### PS1 Prompt Selection
### set SELECTED_PROMPT to the prompt name, minus 'ROOT_' or 'USER_'
#####################################

SELECTED_PROMPT="WORDY_PROMPT"


#####################################
### Prompt colors
#####################################
BRIGHTGREEN='\[\e[1;32m\]'
GREEN='\[\e[0;32m\]'
LIGHTBLUE='\[\e[1;34m\]'
NORMAL='\[\e[m\]'
RED='\[\e[0;31m\]'

# shawn on home-desktop in ~/projects $
ROOT_WORDY_PROMPT="${RED}\u${NORMAL} on ${RED}\h${NORMAL} in ${LIGHTBLUE}\w${BRIGHTGREEN} # ${NORMAL} "
USER_WORDY_PROMPT="${GREEN}\u${NORMAL} on ${GREEN}\h${NORMAL} in ${LIGHTBLUE}\w${BRIGHTGREEN} \$ ${NORMAL}"

USER_ID="$(id -u)"
ROOT_ID="0"
if [ $USER_ID = $ROOT_ID ]; then
    export PS1=$(eval "echo \${$(echo ROOT_${SELECTED_PROMPT})}")
else
    export PS1=$(eval "echo \${$(echo USER_${SELECTED_PROMPT})}")
fi

