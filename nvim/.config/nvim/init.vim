call plug#begin('~/.local/share/nvim/plugged')
    " Colors
    Plug 'joshdick/onedark.vim'

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

    " Syntax highlighting
    Plug 'sheerun/vim-polyglot'

    " Syntax checking
    Plug 'neomake/neomake'

    " Git
    Plug 'airblade/vim-gitgutter'
call plug#end()

let g:deoplete#enable_at_startup = 1
let g:SuperTabDefaultCompletionType = "<c-n>"

if (has("autocmd") && !has("gui"))
  let s:white = { "gui": "#ABB2BF", "cterm": "145", "cterm16" : "7" }
  autocmd ColorScheme * call onedark#set_highlight("Normal", { "fg": s:white }) " No `bg` setting
end

colorscheme onedark

let g:lightline = {
  \ 'colorscheme': 'onedark',
  \ }

autocmd BufWritePost init.vim source $MYVIMRC

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

"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (has("nvim"))
    "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif
"For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
"Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
" < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
if (has("termguicolors"))
    set termguicolors
endif
