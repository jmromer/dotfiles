" ---------------- Plugins --------------------------
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }
Plug 'Valloric/YouCompleteMe', { 'do': 'brew unlink python && ./install.sh --clang-completer --gocode-completer && brew link python' }

Plug 'altercation/vim-colors-solarized' " solarized colors
Plug 'ciaranm/detectindent'             " detect indentation level
Plug 'ervandew/supertab'                " context-aware tab-completion
Plug 'itchyny/lightline.vim'            " lightweight, configurable status line
Plug 'jszakmeister/vim-togglecursor'    " toggle cursor between modes
Plug 'mhinz/vim-hugefile'               " disable options for huge files
Plug 'michaeljsmith/vim-indent-object'  " indent-defined textobj
Plug 'pbrisbin/vim-mkdir'               " create subdirectories as needed
Plug 'scrooloose/syntastic'             " hook into syntax style checkers
Plug 'dkprice/vim-easygrep'             " global search-and-replace
Plug 'tpope/vim-eunuch'                 " Unix commands in vim
Plug 'tpope/vim-projectionist'          " granular project configs
Plug 'tpope/vim-vinegar'                " enhancements to netrw
Plug 'vim-scripts/YankRing.vim'         " maintain yank/del history
Plug 'vim-scripts/tComment'             " line: ctrl+//  block: ctrl+/p
Plug 'vitorgalvao/autoswap_mac'         " auto-respond to swap file message
Plug 'wesQ3/vim-windowswap'             " move panes around with <leader>ww

" Tmux, Tmux runners
Plug 'christoomey/vim-tmux-navigator' " navigate with awarenes of vim splits
Plug 'christoomey/vim-tmux-runner'    " send commands to tmux pane
Plug 'janko-m/vim-test'               " runner for rspec, cucumber, et al.
Plug 'tpope/vim-dispatch'             " async builds and test suites
Plug 'tpope/vim-tbone'                " basic tmux support

" Speedy text editing
Plug 'AndrewRadev/splitjoin.vim'   " toggle between single- and multi-line code
Plug 'gavinbeatty/dragvisuals.vim' " move visual blocks / lines around
Plug 'jiangmiao/auto-pairs'        " auto insert closing delimiters
Plug 'junegunn/vim-easy-align'     " text alignment
Plug 'kana/vim-textobj-user'       " custom text objects
Plug 'tommcdo/vim-exchange'        " text exchange operator cx, cxx, X, cxc
Plug 'tpope/vim-repeat'            " repeat last mapped command with `.`
Plug 'tpope/vim-speeddating'       " increment dates, times, and more
Plug 'tpope/vim-surround'          " surround text block with delimiters
Plug 'tpope/vim-unimpaired'        " useful complementary mappings
Plug 'vim-scripts/visualrepeat'    " extends dot operator to visual mode

" Git
Plug 'airblade/vim-gitgutter' " git diff in the gutter
Plug 'tpope/vim-fugitive'     " side-by-side git blame with :Gblame
Plug 'gregsexton/gitv', { 'on': 'Gitv' } " gitk port. requires fugitive

" Code snippets
Plug 'SirVer/ultisnips'   " snippets engine, integrates with YCM
Plug 'honza/vim-snippets' " textmate-style code snippets
Plug 'heavenshell/vim-jsdoc'
Plug 'justinj/vim-react-snippets'

" -------- Language-Specific -----------
Plug 'elixir-lang/vim-elixir',   { 'for': 'elixir' }
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
Plug 'lervag/vim-latex',         { 'for': 'latex' }
Plug 'tpope/vim-haml',           { 'for': 'haml' }

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
Plug 'vim-ruby/vim-ruby' " can't be lazy-loaded with { 'for': 'ruby' }
Plug 'tpope/vim-bundler'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-rake'

" JavaScript
Plug 'marijnh/tern_for_vim', { 'for': 'javascript', 'do': 'npm install' }
Plug 'jelera/vim-javascript-syntax', { 'for': 'javascript' }
Plug 'moll/vim-node', { 'for': 'javascript' }
Plug 'mxw/vim-jsx', { 'for': 'javascript.jsx' }
Plug 'othree/javascript-libraries-syntax.vim', { 'for': 'javascript' }
Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'javascript.jsx'] }

" Swift
Plug 'kballard/vim-swift'

" -------- Configure -----------
Plug 'Konfekt/FastFold'               " optimized folding for large projects
Plug 'ntpeters/vim-better-whitespace' " detect and highlight bad whitespace
Plug 'rking/ag.vim', { 'on': 'Ag' }   " Ag conveniences
Plug 'tpope/vim-obsession'            " continuous session saving :Obsess[!]
Plug 'tpope/vim-rhubarb'              " GitHub extension for fugitive.vim
Plug 'xolox/vim-easytags'             " continuously updated tags
Plug 'xolox/vim-misc'                 " vim plugin utility library
Plug 'xolox/vim-session'              " session management

" -------- Probationary -----------

Plug 'junegunn/vim-peekaboo'
