shopt -s cdspell    # When changing directory small typos can be ignored by bash
shopt -s histappend # Make bash append rather than overwrite the history on disk
shopt -s cmdhist

if ((BASH_VERSINFO[0] >= 4)); then
    shopt -s autocd     # chdir just by typing dirname
    shopt -s dirspell   # fix spelling during tab completion
fi
