call plug#begin('~/.local/share/nvim/plugged')
    Plug 'rbgrouleff/bclose.vim'
    Plug 'tpope/vim-surround'
    Plug 'francoiscabrol/ranger.vim'
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    Plug 'flazz/vim-colorschemes'
call plug#end()

let g:deoplete#enable_at_startup = 1

set number

set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4

set completeopt+=noinsert
