#!/usr/bin/env fish

echo "removing previously generated files and directories: starting..."
	rm -rfv $HOME/.cache/fisher_workdir
	rm -rfv $HOME/.local/share/nvim
	rm -rfv $HOME/.tmux/plugins
echo "removing previously generated files and directories: done"

echo "creating skeleton directory structure: starting..."
	mkdir -p $HOME/.cache/fisher_workdir
	mkdir -p $HOME/.cache/nvim/undo
	mkdir -p $HOME/.config/fish
	mkdir -p $HOME/.config/nvim
	mkdir -p $HOME/.local/share/nvim
	mkdir -p $HOME/.tmux/plugins
echo "creating skeleton directory structure: done"

echo "symlinking files: staritng..."
	ln -svf $HOME/.config/dotfiles/emptyfile    $HOME/.bash_login
	ln -svf $HOME/.config/dotfiles/emptyfile    $HOME/.bash_logout
	ln -svf $HOME/.config/dotfiles/bash.startup $HOME/.bash_profile
	ln -svf $HOME/.config/dotfiles/bash.startup $HOME/.bashrc
	ln -svf $HOME/.config/dotfiles/fish.plugins $HOME/.cache/fisher_workdir/fishfile
	ln -svf $HOME/.config/dotfiles/fish.startup $HOME/.config/fish/config.fish
	ln -svf $HOME/.config/dotfiles/nvim.startup $HOME/.config/nvim/init.vim
	ln -svf $HOME/.config/dotfiles/editorconfig $HOME/.editorconfig
	ln -svf $HOME/.config/dotfiles/emptyfile    $HOME/.profile
	ln -svf $HOME/.config/dotfiles/tmux.startup $HOME/.tmux.conf
echo "symlinking files: done"

echo "Installing fish plugins: starting..."
	curl https://git.io/fisher --location --output $HOME/.cache/fisher_workdir/actual_fisher.fish
	fish -c 'fisher'
echo "Installing fish plugins: done"

echo "Installing tmux plugins: starting..."
	git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm
	fish -c "$HOME/.tmux/plugins/tpm/bin/install_plugins"
echo "Installing tmux plugins: done"

echo "Installing vim plugins: starting..."
	curl https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim --fail --location --create-dirs --output $HOME/.local/share/nvim/site/autoload/plug.vim
	nvim +PlugInstall! +UpdateRemotePlugins +qall
echo "Installing vim plugins: done"
