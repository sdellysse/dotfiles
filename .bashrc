[[ "$-" != *i* ]] && return # If not running interactively, don't do anything

# On windows, make sure the X server is in the startup group
[[ $(os) = 'Windows'                                ]] && export DISPLAY=:0.0

[[ -d $HOME/bin                                     ]] && export PATH=$PATH:$HOME/bin
[[ -d $HOME/.rvm/bin                                ]] && export PATH=$PATH:$HOME/.rvm/bin
[[ -d $HOME/AppData/Roaming/npm                     ]] && export PATH=$PATH:$HOME/AppData/Roaming/npm
[[ -d $HOME/Software/android-sdks/platform-tools    ]] && export PATH=$PATH:$HOME/Software/android-sdks/platform-tools
[[ -d /cygdrive/c/Program\ Files/nodejs             ]] && export PATH=$PATH:/cygdrive/c/Program\ Files/nodejs

[[ -f $HOME/.bash_aliases               ]] && source $HOME/.bash_aliases

# This is the special sauce to make ssh on windows not shitty
if [ $(os) = 'Windows' ]; then
    export SSH_AUTH_SOCK=/tmp/.ssh-socket
    ssh-add -l >/dev/null 2>&1
    if [ $? = 2 ]; then
        rm -f $SSH_AUTH_SOCK
        eval $(ssh-agent -a $SSH_AUTH_SOCK)
        echo $SSH_AGENT_PID > /tmp/.ssh-agent-pid
    fi
fi

shopt -s cdspell    # When changing directory small typos can be ignored by bash
shopt -s histappend # Make bash append rather than overwrite the history on disk

BRIGHTGREEN='\[\e[1;32m\]'
GREEN='\[\e[0;32m\]'
LIGHTBLUE='\[\e[1;34m\]'
NORMAL='\[\e[m\]'
RED='\[\e[0;31m\]'
export PS1="${GREEN}\u${NORMAL} on ${GREEN}\h${NORMAL} in ${LIGHTBLUE}\w${BRIGHTGREEN} \$ ${NORMAL}"
