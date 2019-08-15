function :vsplit --description 'Run program in tmux vsplit'
	tmux split-window -h -- $argv
end
