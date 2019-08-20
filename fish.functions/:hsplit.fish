function :hsplit --description 'Run program in a tmux hsplit'
	tmux split-window -v -- $argv
end
