# ~/.config/dotfiles/fish/functions/asdf.fish

function asdf
	source $ASDF_DATA_DIR/asdf.fish
	command asdf $argv
end
