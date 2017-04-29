call plug#begin('~/.local/share/nvim/plugged')
    " Colors
    Plug 'flazz/vim-colorschemes'

    "Completion
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    Plug 'ervandew/supertab'
call plug#end()

let g:deoplete#enable_at_startup = 1
let g:SuperTabDefaultCompletionType = "<c-n>"

colorscheme delek

set number
set relativenumber

set tabstop=4
set shiftwidth=4
set expandtab

set ignorecase
set smartcase
