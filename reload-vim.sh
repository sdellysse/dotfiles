#!/bin/sh

mkdir -p "${HOME}/tmp/vim"
rm -rf "${HOME}/.vim"
mkdir "${HOME}/.vim"
touch "${HOME}/.vim/1- DO NOT EDIT THIS DIRECTORY"
touch "${HOME}/.vim/2- IT IS A FUNCTION OF vimrc"
vim -c 'PlugInstall | qall'
