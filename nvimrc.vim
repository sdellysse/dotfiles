" Persistent undos are nice, toss them into XDG cache
	set undodir=$XDG_CACHE_HOME/nvim/undo

" Overridden by editorconfig if current file is child of $HOME
	set shiftwidth=2
	set tabstop=2

" Set leader for plugins that clobber bindings
	let mapleader="\\"

" Declare Plugins
	call plug#begin(stdpath('data') .'/vim-plug')
		Plug 'qpkorr/vim-bufkill'
			" Adds :BD, :BW, etc; kills buffer without killing split

		Plug 'ap/vim-buftabline'
			" Replaces tabline with bufferline

		Plug 'svermeulen/vim-cutlass'
			" Vim's delete keys no longer cut, just delete

		Plug 'editorconfig/editorconfig-vim'
			" enable `.editorconfig` file support

		Plug 'dag/vim-fish'
			" Syntax plugin for fish shell

		Plug 'morhetz/gruvbox'
			" color scheme

		Plug 'pangloss/vim-javascript'
			"syntax plugin for javascript lang

		Plug 'scrooloose/nerdtree'
			" File Drawer

		Plug 'Xuyuanp/nerdtree-git-plugin'
			" Add git support to NERDTree file drawer

		Plug 'tpope/vim-sensible'
			" 'more-sensible' vim base config

		Plug 'mhinz/vim-signify'
			" Show per-file changes from VCS in the gutter

		Plug 'christoomey/vim-tmux-navigator'
			" integrate vim and tmux pane switching
	call plug#end()

" When editing a file, always jump to the last cursor position
	augroup BEGIN_RememberCursorPosition
		autocmd!

		autocmd BufReadPost *
		\ if line("'\"") > 0 && line ("'\"") <= line("$") |
		\   execute "normal! g'\"" |
		\ endif
	augroup END

" Visual Options
	silent! colorscheme gruvbox
	set termguicolors
	set number

" Commandline Mappings
	cnoremap w!! execute 'silent! write !sudo tee "%" > /dev/null <bar> edit!'
		" saves the current file via sudo

" Text wrapping
	set nowrap
  augroup BEGIN_Wrap
		autocmd!
		autocmd FileType text     setlocal wrap
		autocmd FileType markdown setlocal wrap
	augroup END

" Escape from everthing with SMASH(jk)
	inoremap jk <ESC>
	inoremap kj <ESC>

" Disable keys
	nnoremap s <NOP>
		" trying to retrain s->cl

" Buffers instead of tabs
  set hidden
	nnoremap <SPACE>bw :BW<CR>
	nnoremap <SPACE>bn :bnext<CR>
	nnoremap <SPACE>bp :bprev<CR>

" NERDTree Options
	nnoremap <SPACE>ni :NERDTreeFocus<CR>
	nnoremap <SPACE>nx :NERDTreeClose<CR>
	nnoremap <SPACE>nf :NERDTreeFind<CR>
	nnoremap <SPACE>nv :NERDTreeVCS<CR>
	let g:NERDTreeBookmarksFile    = stdpath('data') . '/nerdtreebookmarks'
	let g:NERDTreeIgnore           = [ 'NTUSER.DAT.*', 'ntuser.dat.*', 'ntuser.ini', 'ntuser.pol', 'desktop.ini' ]
	let g:NERDTreeAutoDeleteBuffer = 1
	let g:NERDTreeMinimalUI        = 1
	let g:NERDTreeDirArrows        = 1

" Cutlass settings
	xnoremap x d
	xnoremap X D
		" `x` and `X` now actually cut, the other operations just delete

" Clipboard
	set clipboard=unnamedplus
		" sets the default clipboard to the system clipboard
