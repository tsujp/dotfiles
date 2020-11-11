" plugins
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-surround'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

call plug#end()

" whitespace
:set tabstop=2
:set softtabstop=2
:set shiftwidth=2
:set expandtab

" ui
:set relativenumber
:set mouse=a
:set number

" Live replace
" :set inccommand=split

" disable old regex engine
:set re=0

" fnamemodify(expand("$MYVIMRC"), ":p:h")
