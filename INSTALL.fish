#!/usr/bin/fish

set d (dirname (readlink -f (status --current-filename)))

ln -svf $d/intentionally_blank.sh $HOME/.bash_login
ln -svf $d/intentionally_blank.sh $HOME/.bash_logout
ln -svf $d/bash_noninteractive.sh $HOME/.bash_profile
ln -svf $d/editorconfig           $HOME/.editorconfig
ln -svf $d/emacs-init.el          $HOME/.emacs
ln -svf $d/intentionally_blank.sh $HOME/.profile
ln -svf $d/vimrc                  $HOME/.vimrc

rm -rf $HOME/.config/fish; ln -svf $d/fish $HOME/.config/fish

mkdir -p $HOME/tmp/vim
rm -rf $HOME/.vim
mkdir $HOME/.vim
touch "$HOME/.vim/1- DO NOT EDIT THIS DIRECTORY"
touch "$HOME/.vim/2- IT IS A FUNCTION OF vimrc"
vim -c 'PlugInstall | qall'
