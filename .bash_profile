# ~/.bash_profile: executed by bash(1) for login shells.

# Set PATH so it includes user's private bin if it exists
[[ -d "${HOME}/bin" ]] && PATH="${HOME}/bin:${PATH}"

# Set MANPATH so it includes users' private man if it exists
[[ -d "${HOME}/man" ]] && MANPATH="${HOME}/man:${MANPATH}"

# source the users bashrc if it exists
[[ -f "${HOME}/.bashrc" ]] && source "${HOME}/.bashrc"

# Load RVM into a shell session *as a function*
[[ -s "${HOME}/.rvm/scripts/rvm" ]] && source "${HOME}/.rvm/scripts/rvm"
