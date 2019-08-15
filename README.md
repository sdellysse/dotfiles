Install `fish`, `neovim`, and `tmux`.

Leave user's `$SHELL` set to `bash`; `fish`, to it's advantage, isn't fully
POSIX compatible, so it can cause issues with certain utilities if set as the
default shell. `bash` will automatically start `fish` as needed.

Clone this repo to `~/.config/dotfiles` and run `~/.config/dotfiles/install.fish`.

Notable things:
  - Copy/paste across W10/`vim`/`tmux` are all seamless
	- `wslexec.vbs` make it easy to create W10 shortcuts to X11 apps
		- `tmux` inside `gnome-terminal`: `wslexec.vbs -- env DISPLAY=:0 -- dbus-launch -- gnome-terminal -- tmux`
		- `neovim-qt`: `wslexec.vbs -- env DISPLAY=:0 -- nvim-qt`
	- TODO All `tmux` windows point to different parts of a shared group session
