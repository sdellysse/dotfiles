export EDITOR="vim"
export HISTFILE="${HOME}/tmp/bash_history"

export NVM_DIR="$HOME/.nvm"
if [ -d "${HOME}/.nvm" ]; then
	source "$NVM_DIR/nvm.sh"
	source "$NVM_DIR/bash_completion"
fi
