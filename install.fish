#!/usr/bin/fish

set d (dirname (readlink -f (status --current-filename)))

rm -rf $HOME/.config/fish; ln -svf $d/fish $HOME/.config/fish

ln -svf $d/intentionally_blank.sh $HOME/.bash_login
ln -svf $d/intentionally_blank.sh $HOME/.bash_logout
ln -svf $d/bash_noninteractive.sh $HOME/.bash_profile
ln -svf $d/editorconfig           $HOME/.editorconfig
ln -svf $d/emacs.el               $HOME/.emacs
ln -svf $d/intentionally_blank.sh $HOME/.profile

mkdir --parents ~/.config/nvim && ln -svf $d/nvimrc.vim $HOME/.config/nvim/init.vim
#ln -svf $d/vimrc $HOME/.vimrc
#mkdir -p $HOME/tmp/vim
#rm -rf $HOME/.vim
#mkdir $HOME/.vim
#touch "$HOME/.vim/1- DO NOT EDIT THIS DIRECTORY"
#touch "$HOME/.vim/2- IT IS A FUNCTION OF vimrc"
#vim -c 'PlugInstall | qall'
