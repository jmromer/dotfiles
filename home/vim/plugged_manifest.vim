" ---------------- Plugins --------------------------
" * Use single quotes
" * Plugin options
"   - Plug 'nsf/gocode', { 'tag': 'v.20150303', 'rtp': 'vim' }
" * Plugin outside ~/.vim/plugged with post-update hook
"   - Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }
" * Unmanaged plugin (manually installed and updated)
"   - Plug '~/my-prototype-plugin'

" --- On-Demand Loading ---
" NERD tree will be loaded on the first invocation of NERDTreeToggle command
" Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }

" Multiple commands
" Plug 'junegunn/vim-github-dashboard', { 'on': ['GHDashboard', 'GHActivity'] }

" Loaded when clojure file is opened
" Plug 'tpope/vim-fireplace', { 'for': 'clojure' }

" Multiple file types
" Plug 'kovisoft/paredit', { 'for': ['clojure', 'scheme'] }

" On-demand loading on both conditions
" Plug 'junegunn/vader.vim',  { 'on': 'Vader', 'for': 'vader' }"
" -----------------------------------------------------------

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }
Plug 'Valloric/YouCompleteMe', { 'do': './install.sh' }

Plug 'altercation/vim-colors-solarized' " solarized colors
Plug 'ciaranm/detectindent'             " detect indentation level
Plug 'ervandew/supertab'                " context-aware tab-completion
Plug 'itchyny/lightline.vim'            " lightweight, configurable status line
Plug 'jszakmeister/vim-togglecursor'    " toggle cursor between modes
Plug 'mhinz/vim-hugefile'               " disable options for huge files
Plug 'michaeljsmith/vim-indent-object'  " indent-defined textobj
Plug 'pbrisbin/vim-mkdir'               " create subdirectories as needed
Plug 'scrooloose/syntastic'             " hook into syntax style checkers
Plug 'tpope/vim-bundler'                " Bundler commands
Plug 'tpope/vim-eunuch'                 " Unix commands in vim
Plug 'tpope/vim-vinegar'                " enhancements to netrw
Plug 'vim-scripts/YankRing.vim'         " maintain yank/del history
Plug 'vim-scripts/tComment'             " line: ctrl+//  block: ctrl+/p
Plug 'vitorgalvao/autoswap_mac'         " auto-respond to swap file message
Plug 'wesQ3/vim-windowswap'             " move panes around with <leader>ww

" Configure
Plug 'Konfekt/FastFold'                " optimized folding for large projects
Plug 'ntpeters/vim-better-whitespace'   " detect and highlight bad whitespace
Plug 'rking/ag.vim'                     " Ag conveniences
Plug 'tpope/vim-obsession'              " continuous session saving :Obsess[!]
Plug 'tpope/vim-rhubarb'                " GitHub extension for fugitive.vim
Plug 'xolox/vim-easytags'              " continuously updated tags
Plug 'xolox/vim-misc'                  " vim plugin utility library
Plug 'xolox/vim-session'               " session management

" Ruby, Rails
Plug 'ecomba/vim-ruby-refactoring'    " keybindings for refactoring
Plug 'nelstrom/vim-textobj-rubyblock' " ruby block text objects
Plug 'tpope/vim-endwise'              " add 'end' in ruby et al
Plug 'tpope/vim-rails'                " Rails support
Plug 'tpope/vim-rbenv'                " Rbenv support
Plug 'vim-ruby/vim-ruby'              " Ruby support

" JavaScript, CoffeeScript
Plug 'jelera/vim-javascript-syntax'           " JavaScript Syntax definitions
Plug 'kchmck/vim-coffee-script'               " CoffeeScript syntax defintions
Plug 'marijnh/tern_for_vim', { 'do': 'npm install' } " JavaScript ctags
Plug 'moll/vim-node'                          " Node support
Plug 'othree/javascript-libraries-syntax.vim' " syntax for popular JS libraries

" Tmux / runners
Plug 'christoomey/vim-tmux-navigator' " navigate with awarenes of vim splits
Plug 'christoomey/vim-tmux-runner'    " send commands to tmux pane
Plug 'janko-m/vim-test'               " runner for rspec, cucumber, et al.
Plug 'tpope/vim-tbone'                " basic tmux support
Plug 'tpope/vim-dispatch'             " async builds and test suites
Plug 'tpope/vim-rake'                 " Rake wrapper

" Misc languages
Plug 'elixir-lang/vim-elixir'           " Elixir syntax
Plug 'lervag/vim-latex'                 " LaTeX syntax
Plug 'mustache/vim-mustache-handlebars' " Mustache template syntax
Plug 'tpope/vim-cucumber'               " Cucumber syntax
Plug 'tpope/vim-haml'                   " Haml syntax

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

" Code snippets
Plug 'SirVer/ultisnips'   " snippets engine, integrates with YCM
Plug 'honza/vim-snippets' " textmate-style code snippets

" Git
Plug 'airblade/vim-gitgutter' " git diff in the gutter
Plug 'tpope/vim-fugitive'     " side-by-side git blame with :Gblame


" -------- Probationary -----------

" Probationary / Async candidates
" Plug 'Lokaltog/vim-easymotion'         " jump without numbers
" Plug 'gregsexton/gitv'                 " gitk port. requires fugitive
" Plug 'majutsushi/tagbar'               " navigate a list of methods / classes
" Plug 'sandeepcr529/Buffet.vim'         " user-friendly buffer list
" Plug 'sjl/gundo.vim'                   " graphical undo tree
" Plug 'skwp/greplace.vim'               " global search-and-replace
" Plug 'vim-scripts/scratch.vim'         " scratch buffer
" Plug 'vim-scripts/c.vim'                " C syntax and shortcuts
" Plug 'vim-scripts/ReplaceWithRegister' " Replace with contents of a register
" Plug 'vim-scripts/ReplaceWithSameIndentRegister'

" Clojure
" Plug 'tpope/vim-fireplace'    " clojure REPL support
" Plug 'tpope/vim-leiningen'    " vim support for Leiningen
" Plug 'venantius/vim-cljfmt'   " clojure auto-formatter
" Plug 'venantius/vim-eastwood' " clojure linter
" :dependencies [[jonase/eastwood "0.2.1" :exclusions [org.clojure/clojure]]]

