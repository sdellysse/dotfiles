set guioptions=agit

if has('gui_macvim')
    set macligatures
    set guifont=Fira\ Code:h12
endif
if has('win32') || has('win64')
    set guifont=Consolas:h11
endif
