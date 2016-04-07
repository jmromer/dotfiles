" ---------------- Plugins --------------------------
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }
Plug 'Valloric/YouCompleteMe', { 'do': './install.py --clang-completer --gocode-completer' }

Plug 'dkprice/vim-easygrep'             " global search-and-replace
Plug 'itchyny/lightline.vim'            " lightweight, configurable status line
Plug 'jiangmiao/auto-pairs'             " automatic delimiter closing
Plug 'jszakmeister/vim-togglecursor'    " toggle cursor between modes
Plug 'junegunn/vim-peekaboo'            " register preview
Plug 'kien/rainbow_parentheses.vim'     " delimiter highlighting
Plug 'mhinz/vim-hugefile'               " disable options for huge files
Plug 'ntpeters/vim-better-whitespace'   " detect and highlight bad whitespace
Plug 'pbrisbin/vim-mkdir'               " create subdirectories as needed
Plug 'rizzatti/dash.vim'                " search Dash documentation
Plug 'tpope/vim-eunuch'                 " Unix commands in vim
Plug 'tpope/vim-git'                    " git commit filetype settings, et al
Plug 'tpope/vim-rsi'                    " smart readline bindings in cmode et al
Plug 'tpope/vim-sleuth'                 " detect indentation level
Plug 'tpope/vim-vinegar'                " enhancements to netrw
Plug 'vim-scripts/netrw.vim'            " older (bug-free) version of netrw
Plug 'vitorgalvao/autoswap_mac'         " auto-respond to swap file message
Plug 'wesQ3/vim-windowswap'             " move panes around with <leader>ww
Plug 'xolox/vim-easytags'               " automatically updated ctags
Plug 'xolox/vim-misc' | Plug 'xolox/vim-session' " session management
Plug 'xolox/vim-reload'                 " reload vimrc files automatically

" Speedy text editing
Plug 'AndrewRadev/splitjoin.vim' " toggle between single- and multi-line code
Plug 'junegunn/vim-easy-align'   " text alignment
Plug 'tpope/vim-repeat'          " repeat last mapped command with `.`
Plug 'tpope/vim-speeddating'     " increment dates, times, and more
Plug 'tpope/vim-unimpaired'      " useful complementary mappings
Plug 'vim-scripts/visualrepeat'  " extends dot operator to visual mode
Plug 'zirrostig/vim-schlepp'     " move visual blocks / lines around

" Text objects
Plug 'kana/vim-textobj-entire' " Adds ae/ie
Plug 'kana/vim-textobj-indent' " Adds i
Plug 'kana/vim-textobj-user'   " custom text objects
Plug 'kana/vim-textobj-line'   " Adds l

" Motions
Plug 'christoomey/vim-sort-motion'     " gs[ai][p({]
Plug 'christoomey/vim-system-copy'     " cp
Plug 'tommcdo/vim-exchange'            " text exchange operator cx, cxx, X, cxc
Plug 'tomtom/tcomment_vim'             " gcc / gc[ai][pim]
Plug 'tpope/vim-surround'              " surround text block with delimiters
Plug 'vim-scripts/ReplaceWithRegister' " griw

" Tmux, Tmux runners
Plug 'christoomey/vim-tmux-navigator' " navigate with awarenes of vim splits
Plug 'tpope/vim-tbone'                " basic tmux support
Plug 'benmills/vimux'                 " interact with tmux from vim
Plug 'janko-m/vim-test'               " runner for rspec, cucumber, et al.
Plug 'bswinnerton/vim-test-github'    " vim-test extension for github

" Git
Plug 'airblade/vim-gitgutter' " git diff in the gutter
Plug 'tpope/vim-fugitive'     " side-by-side git blame with :Gblame

" Code snippets
Plug 'SirVer/ultisnips' " snippets engine, integrates with YCM

" -------- Language-Specific -----------
Plug 'elixir-lang/vim-elixir', { 'for': 'elixir' }
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
Plug 'lervag/vim-latex', { 'for': 'latex' }
Plug 'tpope/vim-haml', { 'for': 'haml' }
Plug 'kballard/vim-swift', { 'for': 'swift' }
Plug 'fatih/vim-go', { 'for': 'go' }

" Scala
Plug 'derekwyatt/vim-scala', { 'for': ['scala', 'sbt.scala'] }
Plug 'ktvoelker/sbt-vim',    { 'for': ['scala', 'sbt.scala'] }

" Ruby
Plug 'ck3g/vim-change-hash-syntax', { 'for': 'ruby' }
Plug 'ecomba/vim-ruby-refactoring', { 'for': 'ruby' }
Plug 'nelstrom/vim-textobj-rubyblock', { 'for': 'ruby' }
Plug 't9md/vim-ruby-xmpfilter', { 'for': 'ruby' }
Plug 'tpope/vim-endwise', { 'for': 'ruby' }
Plug 'tpope/vim-rbenv', { 'for': 'ruby' }
Plug 'noprompt/vim-yardoc', { 'for': 'ruby' }
Plug 'stefanoverna/vim-i18n', { 'for': ['ruby', 'haml', 'slim', 'eruby'] }
Plug 'vim-ruby/vim-ruby' " can't be lazy-loaded with { 'for': 'ruby' }
Plug 'tpope/vim-bundler'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-projectionist' | Plug 'tpope/vim-rake'
Plug 'slim-template/vim-slim', { 'for': 'slim' }

" JavaScript
Plug 'marijnh/tern_for_vim', { 'for': 'javascript', 'do': 'npm install' }
Plug 'jelera/vim-javascript-syntax', { 'for': 'javascript' }
Plug 'moll/vim-node', { 'for': 'javascript' }
Plug 'mxw/vim-jsx', { 'for': 'javascript.jsx' }
Plug 'othree/javascript-libraries-syntax.vim', { 'for': 'javascript' }
Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'dsawardekar/portkey' | Plug 'dsawardekar/ember.vim'

" OCaml
Plug 'the-lambda-church/merlin'
Plug 'OCamlPro/ocp-indent'
Plug 'def-lkb/ocp-indent-vim'

" Elm
Plug 'ElmCast/elm-vim'

" Clojure
Plug 'tpope/vim-sexp-mappings-for-regular-people', { 'for': 'clojure'}
Plug 'tpope/vim-fireplace', { 'for': 'clojure'}

" -------- Configure -----------
Plug 'Konfekt/FastFold'               " optimized folding for large projects
Plug 'rking/ag.vim', { 'on': 'Ag' }   " Ag conveniences
Plug 'tpope/vim-rhubarb'              " GitHub extension for fugitive.vim
Plug 'vim-scripts/YankRing.vim'         " maintain yank/del history

" -------- Probationary -----------
