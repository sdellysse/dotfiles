runtime bundle/vim-pathogen/autoload/pathogen.vim
set nocompatible          "dont emulate vi
let mapleader=" "

filetype off
call pathogen#helptags()
call pathogen#runtime_append_all_bundles()
syntax on
filetype plugin indent on

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

set autowrite

colorscheme vividchalk

"calls sudo and asks for password if necessary
cmap w!! %!sudo tee > /dev/null %

"make vimrc editing easier
map <Leader>v :tabnew ~/.vimrc<CR>:echo "Editing VIMRC"<CR>
map <Leader>V :source ~/.vimrc<CR>:echo "Reloaded VIMRC"<CR>

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
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-h> <C-w>h
map <C-l> <C-w>l

if has("autocmd")
  " When editing a file, always jump to the last cursor position
  autocmd BufReadPost *
  \ if line("'\"") > 0 && line ("'\"") <= line("$") |
  \   exe "normal! g'\"" |
  \ endif

  " Remove trailing whitespace from code files on save
  function! StripTrailingWhitespace()
    " store current cursor location
    silent exe "normal ma<CR>"
    " delete the whitespace (e means don't warn if pattern not found)
    %s/\s\+$//e
    " restore old cursor location
    silent exe "normal `a<CR>"
  endfunction
  autocmd BufWritePre *  call StripTrailingWhitespace()

endif
