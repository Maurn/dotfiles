filetype off
call plug#begin('~/.local/share/nvim/plugged')
    Plug 'flazz/vim-colorschemes'
    Plug 'itchyny/lightline.vim'
    Plug 'jiangmiao/auto-pairs' 
    " Plug 'sheerun/vim-polyglot'
    Plug 'airblade/vim-gitgutter'
    Plug 'jimmay5469/vim-spacemacs'
    Plug 'KSP-KOS/EditorTools', { 'branch': 'develop', 'rtp': 'VIM/vim-kerboscript' }
    " Plug 'nsf/gocode', { 'tag': 'v.20150303', 'rtp': 'vim' }
call plug#end()

colorscheme delek

set noshowmode

set number
set relativenumber

set hidden

set scrolloff=2

set tabstop=4
set shiftwidth=4
set expandtab

set ignorecase
set smartcase

set splitbelow
set splitright
set fillchars+=vert:\│
hi clear VertSplit

let mapleader = "\<Space>"
" nnoremap <Leader>w :w<CR>
" nnoremap <Leader>o :FZF<CR>
" nnoremap <Leader>q :q<CR>

" " Easier split navigation
" nnoremap <C-J> <C-W><C-J>
" nnoremap <C-K> <C-W><C-K>
" nnoremap <C-L> <C-W><C-L>
" nnoremap <C-H> <C-W><C-H>
