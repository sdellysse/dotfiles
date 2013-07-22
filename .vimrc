" Tell Vim as early as possible not to try to emulate vi
set nocompatible

" Make win32-vim look for files in the same places as the regular versions
if has('win32') || has('win64')
    set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
    set viminfo+=n$HOME/.vim/info
endif

" Pathogen is now managed by git too, so tell Vim to load it specifically
runtime bundle/vim-pathogen/autoload/pathogen.vim
filetype off
call pathogen#helptags()
call pathogen#runtime_append_all_bundles()
syntax on
filetype plugin indent on

let mapleader=" "
set background=dark       "make sure vim knows bg is dark
set showmode              "display current mode
set showcmd               "display partially typed commands
set nowrap                "dont wrap long lines
set tabstop=4
set shiftwidth=4          "indent two spaces
set shiftround            "Round indent to multiple of 'shiftwidth'
set expandtab             "insert spaces instead of tab
set ruler                 "turn of position on bottom
set number                "turn on line numbers
set showmatch             "highlight matching brackets
set nobackup              "backups are for wimps
set pastetoggle=<F12>     "press when pasting multiple lines
set backspace=2           "notepad style backspacing
set title                 "set the terminal title to the filename
set scrolloff=9           "start scrolling when hits this many lines from edge
set showtabline=2         "always show tab bar

colorscheme vividchalk

"calls sudo and asks for password if necessary
cmap w!! %!sudo tee > /dev/null %

" Easy out-of-insert-mode bindings
inoremap jk <ESC>
inoremap kj <ESC>

" Disable arrow keys:
inoremap  <Up>     <NOP>
inoremap  <Down>   <NOP>
inoremap  <Left>   <NOP>
inoremap  <Right>  <NOP>
noremap   <Up>     <NOP>
noremap   <Down>   <NOP>
noremap   <Left>   <NOP>
noremap   <Right>  <NOP>

" create new tabs with \N
" close tabs with \C
" modified tabs must be close with :tabclose!
map <Leader>n :tabnew<CR>
map <Leader>c :tabclose<CR>

map <Leader>d :NERDTreeToggle<CR>
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

if has("autocmd")
  " When editing a file, always jump to the last cursor position
  autocmd BufReadPost *
  \ if line("'\"") > 0 && line ("'\"") <= line("$") |
  \   exe "normal! g'\"" |
  \ endif

  " Remove trailing whitespace from code files on save
  function! StripTrailingWhitespace()
    silent exe "normal ma<CR>"
    %s/\s\+$//e
    silent exe "normal `a<CR>"
  endfunction
  autocmd BufWritePre *  call StripTrailingWhitespace()

  " Enforce 2 space indent for ruby files
  autocmd FileType ruby setlocal shiftwidth=2 tabstop=2
endif

au BufNewFile,BufRead *.ejs set filetype=html
au BufNewFile,BufRead *.cljs set filetype=clojure
