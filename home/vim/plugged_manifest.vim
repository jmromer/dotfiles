" ---------------- Plugins --------------------------
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }
Plug 'Valloric/YouCompleteMe', { 'do': './install.py --clang-completer --gocode-completer' }
Plug 'Shougo/vimproc.vim', { 'do': 'make > /dev/null' } " async shell command execution

Plug 'ciaranm/detectindent'             " detect indentation level
Plug 'dkprice/vim-easygrep'             " global search-and-replace
Plug 'itchyny/lightline.vim'            " lightweight, configurable status line
Plug 'jiangmiao/auto-pairs'             " automatic delimiter closing
Plug 'jszakmeister/vim-togglecursor'    " toggle cursor between modes
Plug 'junegunn/vim-peekaboo'            " register preview
Plug 'mhinz/vim-hugefile'               " disable options for huge files
Plug 'michaeljsmith/vim-indent-object'  " indent-defined textobj
Plug 'pbrisbin/vim-mkdir'               " create subdirectories as needed
Plug 'tpope/vim-eunuch'                 " Unix commands in vim
Plug 'tpope/vim-git'                    " git commit filetype settings, et al
Plug 'tpope/vim-rsi'                    " smart readline bindings in cmode et al
Plug 'tpope/vim-vinegar'                " enhancements to netrw
Plug 'vim-scripts/YankRing.vim'         " maintain yank/del history
Plug 'vim-scripts/tComment'             " line: ctrl+//  block: ctrl+/p
Plug 'vitorgalvao/autoswap_mac'         " auto-respond to swap file message
Plug 'wesQ3/vim-windowswap'             " move panes around with <leader>ww
Plug 'xolox/vim-easytags'               " automatically updated ctags
Plug 'xolox/vim-session'                " session management

" Tmux, Tmux runners
Plug 'christoomey/vim-tmux-navigator' " navigate with awarenes of vim splits
Plug 'christoomey/vim-tmux-runner'    " send commands to tmux pane
Plug 'janko-m/vim-test'               " runner for rspec, cucumber, et al.
Plug 'tpope/vim-dispatch'             " async builds and test suites
Plug 'tpope/vim-tbone'                " basic tmux support

" Speedy text editing
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

" Git
Plug 'airblade/vim-gitgutter' " git diff in the gutter
Plug 'tpope/vim-fugitive'     " side-by-side git blame with :Gblame
Plug 'gregsexton/gitv', { 'on': 'Gitv' } " gitk port. requires fugitive

" Code snippets
Plug 'SirVer/ultisnips'        " snippets engine, integrates with YCM
Plug 'heavenshell/vim-jsdoc'   " jsdoc
Plug 'justinj/vim-react-snippets'

" -------- Language-Specific -----------
Plug 'elixir-lang/vim-elixir', { 'for': 'elixir' }
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
Plug 'lervag/vim-latex', { 'for': 'latex' }
Plug 'tpope/vim-haml', { 'for': 'haml' }
Plug 'kballard/vim-swift', { 'for': 'swift' }

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
Plug 'ngmy/vim-rubocop', { 'for': ['ruby', 'ruby.rspec', 'eruby'] }
Plug 'noprompt/vim-yardoc', { 'for': 'ruby' }
Plug 'stefanoverna/vim-i18n', { 'for': ['ruby', 'haml', 'slim', 'eruby'] }
Plug 'vim-ruby/vim-ruby' " can't be lazy-loaded with { 'for': 'ruby' }
Plug 'tpope/vim-bundler'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-projectionist' | Plug 'tpope/vim-rake'
Plug 'slim-template/vim-slim'

" JavaScript
Plug 'marijnh/tern_for_vim', { 'for': 'javascript', 'do': 'npm install' }
Plug 'jelera/vim-javascript-syntax', { 'for': 'javascript' }
Plug 'moll/vim-node', { 'for': 'javascript' }
Plug 'mxw/vim-jsx', { 'for': 'javascript.jsx' }
Plug 'othree/javascript-libraries-syntax.vim', { 'for': 'javascript' }
Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'dsawardekar/portkey' | Plug 'dsawardekar/ember.vim'

" -------- Configure -----------
Plug 'Konfekt/FastFold'               " optimized folding for large projects
Plug 'ntpeters/vim-better-whitespace' " detect and highlight bad whitespace
Plug 'rking/ag.vim', { 'on': 'Ag' }   " Ag conveniences
Plug 'tpope/vim-rhubarb'              " GitHub extension for fugitive.vim
Plug 'xolox/vim-misc'                 " vim plugin utility library

" -------- Probationary -----------
