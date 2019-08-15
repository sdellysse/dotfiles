#!/usr/bin/fish

echo "removing previously generated files and directories: starting..."
	rm -rfv $HOME/.config/fish/functions
	rm -rfv $HOME/.config/fish/completions
	rm -rfv $HOME/.config/fish/conf.d
	rm -rfv $HOME/.cache/fisher_workdir
	rm -rfv $HOME/.local/share/nvim
	rm -rfv $HOME/.tmux/plugins
echo "removing previously generated files and directories: done"

echo "creating skeleton directory structure: starting..."
	mkdir --verbose --parents $HOME/.cache/fisher_workdir
	mkdir --verbose --parents $HOME/.cache/nvim/undo
	mkdir --verbose --parents $HOME/.config/nvim
	mkdir --verbose --parents $HOME/.local/share/nvim
	mkdir --verbose --parents $HOME/.tmux/plugins
echo "creating skeleton directory structure: done"

echo "symlinking files: staritng..."
	set __DIR__ (dirname (readlink -f (status --current-filename)))
	ln -svf $__DIR__/emptyfile        $HOME/.bash_login
	ln -svf $__DIR__/emptyfile        $HOME/.bash_logout
	ln -svf $__DIR__/startup.bash     $HOME/.bash_profile
	ln -svf $__DIR__/startup.bash     $HOME/.bashrc
	ln -svf $__DIR__/fish/fisher      $HOME/.cache/fisher_workdir/fishfile
	ln -svf $__DIR__/fish/config.fish $HOME/.config/fish/config.fish
	ln -svf $__DIR__/fish/functions   $HOME/.config/fish/functions
	ln -svf $__DIR__/fish/completions $HOME/.config/fish/completions
	ln -svf $__DIR__/fish/conf.d      $HOME/.config/fish/conf.d
	ln -svf $__DIR__/nvimrc.vim       $HOME/.config/nvim/init.vim
	ln -svf $__DIR__/editorconfig     $HOME/.editorconfig
	ln -svf $__DIR__/emptyfile        $HOME/.profile
	ln -svf $__DIR__/tmux.conf        $HOME/.tmux.conf
echo "symlinking files: done"

echo "Installing fish plugins: starting..."
	curl https://git.io/fisher --location --output $HOME/.cache/fisher_workdir/actual_fisher.fish
	builtin source $HOME/.cache/fisher_workdir/actual_fisher.fish
	fisher
echo "Installing fish plugins: done"

echo "Installing tmux plugins: starting..."
	git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm
	$HOME/.tmux/plugins/tpm/bin/install_plugins
echo "Installing tmux plugins: done"

echo "Installing vim plugins: starting..."
	curl https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim --fail --location --create-dirs --output $XDG_DATA_HOME/nvim/site/autoload/plug.vim
	nvim +PlugInstall! +qall
echo "Installing vim plugins: done"
