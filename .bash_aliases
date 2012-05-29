#!/bin/bash

if [ $OS != 'OSX' ]; then
    alias ls="/bin/ls --color=always -F"
else
    alias ls="/bin/ls -GF"
fi
alias l="ls -l"
alias la="l -a"

alias bigworm="ssh bigworm"
alias propcap="ssh propertycapsule"
