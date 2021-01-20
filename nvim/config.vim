" don't bother being compatible with vi, only vim stuff pl0x
setglobal nocompatible

let g:mapleader=","

" whitespace
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab

" ui
set relativenumber
set mouse=a
set number

" first two force status line and tab line to always display, last disables
" GUI tab in favour of plain text version
set laststatus=2
set showtabline=2
set guioptions-=e

" disable old regex engine
set re=0
