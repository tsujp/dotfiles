" plugins
call plug#begin('~/.vim/plugged')

  " theme
  " Plug 'morhetz/gruvbox'
  Plug 'junegunn/seoul256.vim'

  " js/ts/jsx/tsx ecosystem
  Plug 'pangloss/vim-javascript'
  Plug 'leafgarland/typescript-vim'
  Plug 'maxmellon/vim-jsx-pretty'
  Plug 'peitalin/vim-jsx-typescript'

  " split-view directory tree (using default netrw)
  Plug 'tpope/vim-vinegar'

  " fuzzy find
  Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
  Plug 'junegunn/fzf.vim'

  Plug 'ludovicchabant/vim-gutentags'   " ctags
  Plug 'airblade/vim-rooter'            " automatically scopes vim wd to dir with .git
  Plug 'tpope/vim-commentary'           " easily comment in/out things
  Plug 'machakann/vim-highlightedyank'  " hilight yanked lights briefly
  Plug 'justinmk/vim-sneak'             " quickly go to locations in files
  Plug 'mhinz/vim-startify'             " startup screen
  Plug 'easymotion/vim-easymotion'      " shortcut key-literals for vim motions
  Plug 'nathanaelkane/vim-indent-guides'
  " Plug 'norcalli/nvim-colorizer.lua'    " colour swatch-preview
  Plug 'junegunn/rainbow_parentheses.vim'
  " tabular for whitespace align?

  Plug 'tpope/vim-abolish'
  Plug 'tpope/vim-unimpaired'
  Plug 'tpope/vim-fugitive'

  " coc: lsp
  Plug 'neoclide/coc.nvim', { 'branch': 'release' }
  let g:coc_global_extensions = [
    \ 'coc-tsserver'
  \ ]

call plug#end()
