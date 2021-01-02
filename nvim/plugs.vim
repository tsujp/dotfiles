" plugs
call plug#begin('~/.vim/plugged')

  " theme
  Plug 'phanviet/vim-monokai-pro'

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

  " git
  Plug 'tpope/vim-fugitive'               " make calls to Git from Vim
  Plug 'tpope/vim-rhubarb'                " github extension for fugitive
                                          " I should write a sourcehut extension for fugitive
  Plug 'mhinz/vim-signify'                " show vcs diff symbols in sign column
  Plug 'junegunn/gv.vim'                  " browse git commits inside Vim
  " lazy git here

  Plug 'liuchengxu/vim-which-key' " define _and_ show leader key binding helpers
  Plug 'voldikss/vim-floaterm' " open _stuff_ in a floating (overlayed) terminal
  Plug 'godlygeek/tabular'                " line up me text, son
  Plug 'ludovicchabant/vim-gutentags'     " ctags
  Plug 'airblade/vim-rooter'              " automatically scopes Vim work dir to parent(folder) .git
  Plug 'tpope/vim-commentary'             " easily comment in/out things
  Plug 'machakann/vim-highlightedyank'    " hilight yanked lights briefly
  Plug 'justinmk/vim-sneak'               " quickly go to locations in files
  Plug 'mhinz/vim-startify'               " startup screen
  Plug 'easymotion/vim-easymotion'        " shortcut key-literals for vim motions
  Plug 'nathanaelkane/vim-indent-guides'
  Plug 'junegunn/rainbow_parentheses.vim'
                                          " Plug 'norcalli/nvim-colorizer.lua'                " colour swatch-preview

  Plug 'tpope/vim-abolish'
  Plug 'tpope/vim-unimpaired'

  " coc: lsp
  Plug 'neoclide/coc.nvim', { 'branch': 'release' }
  let g:coc_global_extensions = [
    \ 'coc-tsserver'
  \]

call plug#end()
