#!/usr/bin/env fish

echo "copying files: staritng..."
	# Need the rm here because fish will recreate defaults as soon as it gets control back
	rm -rfv $HOME/.config/fish && cp -fvr $HOME/.config/dotfiles/fish $HOME/.config/fish

	cp -fv $HOME/.config/dotfiles/bash/emptyfile  $HOME/.bash_login
	cp -fv $HOME/.config/dotfiles/bash/emptyfile  $HOME/.bash_logout
	cp -fv $HOME/.config/dotfiles/bash/startup.sh $HOME/.bash_profile
	cp -fv $HOME/.config/dotfiles/bash/startup.sh $HOME/.bashrc
	cp -fv $HOME/.config/dotfiles/editorconfig    $HOME/.editorconfig
	cp -fv $HOME/.config/dotfiles/bash/emptyfile  $HOME/.profile
echo "copying files: done"

if [ -e $HOME/.cache/asdf ]
	echo "ASDF already installed; skipping install"
else 
	echo "Installing ASDF: starting..."
	git clone https://github.com/asdf-vm/asdf $HOME/.cache/asdf
	echo "Installing ASDF: done"
end

echo "Updating ASDF: starting..."
pushd $HOME/.cache/asdf
	git checkout master
	git branch -D latest
	git pull
	git fetch --tags
	echo "Switching to version" ( git describe --tags ( git rev-list --tags --max-count=1) )
	git checkout ( git describe --tags ( git rev-list --tags --max-count=1) ) -b latest
popd
echo "Updating ASDF: done"

echo "Installing fish plugins: starting..."
	curl https://git.io/fisher --location --output $HOME/.config/fish/functions/fisher.fish
	fish -c 'fisher'
echo "Installing fish plugins: done"

echo "Creating fuck.fish: starting..."
	thefuck --alias > $HOME/.config/fish/functions/fuck.fish
echo "Creating fuck.fish: done"