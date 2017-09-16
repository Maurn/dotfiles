call plug#begin('~/.local/share/nvim/plugged')
    " Colors
    Plug 'flazz/vim-colorschemes'

    " Statusline
    Plug 'itchyny/lightline.vim'

    " File browsing
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
    Plug 'junegunn/fzf.vim'
    
    " Completion
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    Plug 'ervandew/supertab'
    Plug 'jiangmiao/auto-pairs' 
    Plug 'zchee/deoplete-zsh'
    Plug 'vimwiki/vimwiki'

    " Syntax highlighting
    Plug 'sheerun/vim-polyglot'

    " Syntax checking
    Plug 'neomake/neomake'

    " Git
    Plug 'airblade/vim-gitgutter'
call plug#end()

let g:deoplete#enable_at_startup = 1
let g:SuperTabDefaultCompletionType = "<c-n>"

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
nnoremap <Leader>w :w<CR>
nnoremap <Leader>o :FZF<CR>
nnoremap <Leader>q :q<CR>

" Easier split navigation
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
