call plug#begin('~/.local/share/nvim/plugged')
    Plug 'scrooloose/nerdtree'
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
call plug#end()

let g:deoplete#enable_at_startup = 1

set number

set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4
