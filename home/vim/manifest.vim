" ---------------- Plugins --------------------------
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }

if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif

Plug 'w0rp/ale'
" Plug 'benekastah/neomake'              " async linting, et al. best with nvim

Plug 'Shougo/neco-vim'
Plug 'zchee/deoplete-clang'
Plug 'Shougo/deoplete-rct'
Plug 'zchee/deoplete-zsh'
Plug 'wellle/tmux-complete.vim'
Plug 'SevereOverfl0w/deoplete-github'

Plug 'kien/rainbow_parentheses.vim'    " delimiter highlighting
Plug 'ntpeters/vim-better-whitespace'  " detect and highlight bad whitespace
Plug 'tpope/vim-surround'              " surround text block with delimiters
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'shumphrey/fugitive-gitlab.vim'
Plug 'tomtom/tcomment_vim'             " gcc / gc[ai][pim]
Plug 'christoomey/vim-system-copy'     " cp
Plug 'christoomey/vim-sort-motion'
Plug 'itchyny/lightline.vim'           " lightweight, configurable status line
Plug 'SirVer/ultisnips'                " snippets engine, integrates with YCM
Plug 'vim-scripts/YankRing.vim'        " maintain yank/del history
Plug 'tpope/vim-vinegar'              " enhancements to netrw
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-git'                  " git commit filetype settings, et al
Plug 'mattn/emmet-vim'
Plug 'jiangmiao/auto-pairs'
Plug 'haya14busa/incsearch.vim'
Plug 'jmromer/yankee.vim'
Plug 'mhinz/vim-hugefile'

Plug 'jszakmeister/vim-togglecursor'  " toggle cursor between modes
Plug 'pbrisbin/vim-mkdir'             " create subdirectories as needed

" Text objects
Plug 'kana/vim-textobj-entire' " Adds ae/ie
Plug 'kana/vim-textobj-indent' " Adds i
Plug 'kana/vim-textobj-user'   " custom text objects
Plug 'kana/vim-textobj-line'   " Adds l

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
Plug 't9md/vim-ruby-xmpfilter', { 'for': 'ruby' }
Plug 'tpope/vim-rails', { 'for': ['ruby']  }

" JS
Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }
Plug 'wizicer/vim-jison'
Plug 'elzr/vim-json'

" Python
Plug 'cjrh/vim-conda'
Plug 'jeetsukumaran/vim-pythonsense'
Plug 'plytophogy/vim-virtualenv'
Plug 'python-mode/python-mode', { 'branch': 'develop' }
Plug 'tweekmonster/django-plus.vim'
Plug 'vim-scripts/indentpython.vim'
Plug 'zchee/deoplete-jedi'
