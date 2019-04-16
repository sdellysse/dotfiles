" Tell Vim as early as possible not to try to emulate vi
set nocompatible


" Vim needs posix
if &shell =~# 'fish$'
  set shell=bash
endif

" Automatically installs the plugin manager if it doesn't exist, and installs
" all plugins after that.
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/0.9.0/plug.vim
  autocmd VimEnter * PlugUpdate
endif

" Set up vim plugins
call plug#begin('~/.vim/plugged')
Plug 'scrooloose/nerdtree'
"Plug 'jamessan/vim-gnupg'
"Plug 'vim-airline/vim-airline'
"Plug 'airblade/vim-gitgutter'
Plug 'sjl/gundo.vim'
Plug 'matchit.zip'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'godlygeek/tabular'
Plug 'editorconfig/editorconfig-vim'

" Syntax
"Plug 'pearofducks/ansible-vim'
Plug 'JulesWang/css.vim',             { 'for': 'css' }
"Plug 'elixir-lang/vim-elixir',        { 'for': 'elixir' }
"Plug 'vim-erlang/vim-erlang-runtime', { 'for': 'erlang' }
"Plug 'fatih/vim-go',                  { 'for': 'go' }
"Plug 'neovimhaskell/haskell-vim',     { 'for': 'haskell' }
Plug 'pangloss/vim-javascript',       { 'for': 'javascript' }
Plug 'mxw/vim-jsx',                   { 'for': 'javascript' }
"Plug 'tbastos/vim-lua',               { 'for': 'lua' }
"Plug 'chr4/nginx.vim'
"Plug 'StanAngeloff/php.vim',          { 'for': 'php' }
"Plug 'rust-lang/rust.vim',            { 'for': 'rust' }
"Plug 'derekwyatt/vim-sbt',            { 'for': 'sbt' }
"Plug 'derekwyatt/vim-scala',          { 'for': 'scala' }
"Plug 'stephpy/vim-yaml',              { 'for': 'yaml' }
Plug 'GutenYe/json5.vim',             { 'for': 'json5' }
Plug 'dag/vim-fish',                  { 'for': 'fish' }

" Themes
Plug 'tpope/vim-vividchalk'

Plug 'hoffstein/vim-tsql'
Plug 'vim-scripts/dbext.vim'

call plug#end()

let mapleader=" "
let g:NERDTreeDirArrows = 0
let g:jsx_ext_required = 0
set colorcolumn=81
set background=dark       "make sure vim knows bg is dark
set showmode              "display current mode
set showcmd               "display partially typed commands
set nowrap                "dont wrap long lines
set shiftround            "Round indent to multiple of 'shiftwidth'
set ruler                 "turn of position on bottom
set number                "turn on line numbers
set showmatch             "highlight matching brackets
set nobackup              "backups are for wimps
set pastetoggle=<F12>     "press when pasting multiple lines
set backspace=2           "notepad style backspacing
set title                 "set the terminal title to the filename
set scrolloff=9           "start scrolling when hits this many lines from edge
set showtabline=2         "always show tab bar
set visualbell
set updatetime=250

set shiftwidth=2
set tabstop=2

" Don't complain if the colorscheme isn't set
silent! colorscheme vividchalk

"calls sudo and asks for password if necessary
cmap w!! %!sudo tee > /dev/null '%'

" Smash JK in insert mode to revert to normal mode
inoremap jk <ESC>
inoremap kj <ESC>

" GVim options
set guioptions=agit

if has('gui_macvim')
    set macligatures
    set guifont=Fira\ Code:h12
endif
if has('win32') || has('win64')
    set guifont=Consolas:h11
endif

if exists(':tnoremap')
  " Smash JK in terminal mode to revert to normal mode
  tnoremap jk <C-\><C-N>
  tnoremap kj <C-\><C-N>
endif


map <Leader>n :tabnew<CR>
map <Leader>c :tabclose<CR>

map <Leader>d :NERDTreeToggle<CR>
map <Leader>u :GundoToggle<CR>

" Control+hjkl moves focus to that window
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

let g:gundo_right = 1
let g:gundo_close_on_revert = 1

" When editing a file, always jump to the last cursor position
autocmd BufReadPost *
\ if line("'\"") > 0 && line ("'\"") <= line("$") |
\   exe "normal! g'\"" |
\ endif

let g:dbext_default_SQLSRV_bin = 'sqlcmd'
let g:dbext_default_SQLSRV_cmd_options = ''

silent! source $HOME/.vimrc.local

