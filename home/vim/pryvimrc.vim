" ----------------- Base Settings ------------------------
set nocompatible
filetype plugin indent on

" Don't display 'Pattern not found' messages
set shortmess+=c

" ----------------- Load Plugins ------------------------
call plug#begin('~/.vim/plugged')
Plug 'Valloric/YouCompleteMe', { 'do': './install.py --clang-completer --gocode-completer' }

Plug 'itchyny/lightline.vim'            " lightweight, configurable status line
Plug 'jiangmiao/auto-pairs'             " automatic delimiter closing
Plug 'jszakmeister/vim-togglecursor'    " toggle cursor between modes
Plug 'junegunn/vim-peekaboo'            " register preview
Plug 'michaeljsmith/vim-indent-object'  " indent-defined textobj
Plug 'ntpeters/vim-better-whitespace'   " detect and highlight bad whitespace
Plug 'pbrisbin/vim-mkdir'               " create subdirectories as needed
Plug 'tomtom/tcomment_vim'              " commenting facilities
Plug 'tpope/vim-eunuch'                 " Unix commands in vim
Plug 'tpope/vim-git'                    " git commit filetype settings, et al
Plug 'tpope/vim-rsi'                    " smart readline bindings in cmode et al
Plug 'tpope/vim-sleuth'                 " detect indentation level
Plug 'tpope/vim-vinegar'                " enhancements to netrw
Plug 'vitorgalvao/autoswap_mac'         " auto-respond to swap file message
Plug 'wesQ3/vim-windowswap'             " move panes around with <leader>ww
"
" " Speedy text editing
Plug 'AndrewRadev/splitjoin.vim'   " toggle between single- and multi-line code
Plug 'junegunn/vim-easy-align'     " text alignment
Plug 'kana/vim-textobj-user'       " custom text objects
Plug 'tommcdo/vim-exchange'        " text exchange operator cx, cxx, X, cxc
Plug 'tpope/vim-repeat'            " repeat last mapped command with `.`
Plug 'tpope/vim-speeddating'       " increment dates, times, and more
Plug 'tpope/vim-surround'          " surround text block with delimiters
Plug 'tpope/vim-unimpaired'        " useful complementary mappings
Plug 'vim-scripts/visualrepeat'    " extends dot operator to visual mode
Plug 'zirrostig/vim-schlepp'       " move visual blocks / lines around

" Tmux, Tmux runners
Plug 'christoomey/vim-tmux-navigator' " navigate with awarenes of vim splits
Plug 'tpope/vim-tbone'                " basic tmux support

" Code snippets
Plug 'SirVer/ultisnips'        " snippets engine, integrates with YCM

" Ruby
Plug 'ck3g/vim-change-hash-syntax', { 'for': 'ruby' }
Plug 'ecomba/vim-ruby-refactoring', { 'for': 'ruby' }
Plug 'nelstrom/vim-textobj-rubyblock', { 'for': 'ruby' }
Plug 't9md/vim-ruby-xmpfilter', { 'for': 'ruby' }
Plug 'tpope/vim-endwise', { 'for': 'ruby' }
Plug 'noprompt/vim-yardoc', { 'for': 'ruby' }
Plug 'vim-ruby/vim-ruby' " can't be lazy-loaded with { 'for': 'ruby' }
Plug 'tpope/vim-bundler'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-projectionist' | Plug 'tpope/vim-rake'
Plug 'slim-template/vim-slim'

" -------- Configure -----------
Plug 'vim-scripts/YankRing.vim'         " maintain yank/del history

" -------- Probationary -----------
Plug 'christoomey/vim-tmux-runner'    " send commands to tmux pane
call plug#end()

" ---------------- Overrides  --------------------------

if filereadable('~/.vimrc.local')
  source $HOME/.vimrc.local
endif
