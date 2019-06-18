" Overridden by editorconfig if current file is child of $HOME
	set shiftwidth=2
	set tabstop=2

" Install vim-plug if not exists
	if !filereadable(stdpath('data') . '/site/autoload/plug.vim')
		silent execute '!curl -fLo ' . stdpath('data') . '/site/autoload/plug.vim' . '  --create-dirs "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"'
		autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
	endif

" Declare Plugins
	call plug#begin(stdpath('data') .'/vim-plug')
		Plug 'tpope/vim-sensible'
		Plug 'editorconfig/editorconfig-vim'

		Plug 'tpope/vim-eunuch'
		Plug 'dag/vim-fish'
		Plug 'morhetz/gruvbox'
		Plug 'scrooloose/nerdtree'
		Plug 'Xuyuanp/nerdtree-git-plugin'
		Plug 'mhinz/vim-signify'
		Plug 'jreybert/vimagit'
	call plug#end()

" When editing a file, always jump to the last cursor position
	augroup LastCursorPositionGroup
		autocmd!

		autocmd BufReadPost *
		\ if line("'\"") > 0 && line ("'\"") <= line("$") |
		\   execute "normal! g'\"" |
		\ endif
	augroup END


" Editor options
	let mapleader="\\"
	silent! colorscheme gruvbox
	set nowrap
	set pastetoggle=<F12>
	set number
	set termguicolors
	cnoremap w!! execute 'silent! write !sudo tee "%" > /dev/null <bar> edit!'

" Exit from insert mode
	inoremap jk    <ESC>
	inoremap kj    <ESC>

" Window movement
	nnoremap <C-h> <C-w>h
	nnoremap <C-j> <C-w>j
	nnoremap <C-k> <C-w>k
	nnoremap <C-l> <C-w>l

" NERDTree Options
	nnoremap <SPACE>d :NERDTreeFocus<CR>
	nnoremap <SPACE>D :NERDTreeClose<CR>
	nnoremap <SPACE>f :NERDTreeFind<CR>
	nnoremap <SPACE>g :NERDTreeVCS<CR>
	let g:NERDTreeBookmarksFile    = stdpath('data') . '/nerdtreebookmarks'
	let g:NERDTreeIgnore           = [ 'NTUSER.DAT.*', 'ntuser.dat.*', 'ntuser.ini', 'desktop.ini' ]
	let g:NERDTreeAutoDeleteBuffer = 1
	let g:NERDTreeMinimalUI        = 1
	let g:NERDTreeDirArrows        = 1

" vimagit
	nnoremap <SPACE>m :Magit<CR>

" Terminal
	nnoremap <SPACE>to :edit term://$SHELL<CR>
	nnoremap <SPACE>tt :tabedit term://$SHELL<CR>
	nnoremap <SPACE>ti :split term://$SHELL<CR>
	nnoremap <SPACE>ts :vsplit term://$SHELL<CR>
	tnoremap <ESC><ESC> <C-\><C-n>
	tnoremap jk <C-\><C-n>
	tnoremap kj <C-\><C-n>
	augroup TerminalGroup
		autocmd!
		autocmd TermOpen * setlocal nonumber
	augroup END
