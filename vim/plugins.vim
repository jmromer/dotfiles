call plug#begin('~/.vim/plugins')

Plug 'FelikZ/ctrlp-py-matcher'          " alternative matcher for CtrlP
Plug 'Konfekt/FastFold'                 " optimized folding for large projects
Plug 'Valloric/YouCompleteMe'           " syntax completion
Plug 'altercation/vim-colors-solarized' " solarized colors
Plug 'ciaranm/detectindent'             " detect indentation level
Plug 'ervandew/supertab'                " context-aware tab-completion
Plug 'itchyny/lightline.vim'            " lightweight, configurable status line
Plug 'jszakmeister/vim-togglecursor'    " toggle cursor between modes
Plug 'kien/ctrlp.vim'                   " Rapid file finding
Plug 'majutsushi/tagbar'                " navigate a list of methods / classes
Plug 'mhinz/vim-hugefile'               " disable options for huge files
Plug 'michaeljsmith/vim-indent-object'  " indent-defined textobj
Plug 'ntpeters/vim-better-whitespace'   " detect and highlight bad whitespace
Plug 'pbrisbin/vim-mkdir'               " create subdirectories as needed
Plug 'rking/ag.vim'                     " Ag conveniences
Plug 'sandeepcr529/Buffet.vim'          " user-friendly buffer list
Plug 'scrooloose/syntastic'             " hook into syntax style checkers
Plug 'sjl/gundo.vim'                    " graphical undo tree
Plug 'skwp/greplace.vim'                " global search-and-replace
Plug 'tpope/vim-dispatch'               " async builds and test suites
Plug 'tpope/vim-bundler'                " Bundler commands
Plug 'tpope/vim-eunuch'                 " Unix commands in vim
Plug 'tpope/vim-obsession'              " continuous session saving :Obsess[!]
Plug 'tpope/vim-rhubarb'                " GitHub extension for fugitive.vim
Plug 'tpope/vim-vinegar'                " enhancements to netrw
Plug 'vim-scripts/YankRing.vim'         " maintain yank/del history
Plug 'vim-scripts/scratch.vim'          " scratch buffer
Plug 'vim-scripts/tComment'             " line: ctrl+//  block: ctrl+/p
Plug 'vitorgalvao/autoswap_mac'         " auto-respond to swap file message
Plug 'wesQ3/vim-windowswap'             " move panes around with <leader>ww
Plug 'xolox/vim-easytags'               " continuously updated tags
Plug 'xolox/vim-misc'                   " vim plugin utility library
Plug 'xolox/vim-session'                " session management

" Clojure
Plug 'tpope/vim-fireplace'    " clojure REPL support
Plug 'tpope/vim-leiningen'    " vim support for Leiningen
Plug 'venantius/vim-cljfmt'   " clojure auto-formatter
Plug 'venantius/vim-eastwood' " clojure linter
" :dependencies [[jonase/eastwood "0.2.1" :exclusions [org.clojure/clojure]]]

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
Plug 'marijnh/tern_for_vim'                   " JavaScript ctags
Plug 'moll/vim-node'                          " Node support
Plug 'othree/javascript-libraries-syntax.vim' " syntax for popular JS libraries

" Tmux
Plug 'christoomey/vim-tmux-navigator' " navigate with awarenes of vim splits
Plug 'christoomey/vim-tmux-runner'    " send commands to tmux pane
Plug 'janko-m/vim-test'               " runner for rspec, cucumber, et al.
Plug 'tpope/vim-tbone'                " basic tmux support

" Misc languages
Plug 'elixir-lang/vim-elixir'           " Elixir syntax
Plug 'lervag/vim-latex'                 " LaTeX syntax
Plug 'mustache/vim-mustache-handlebars' " Mustache template syntax
Plug 'tpope/vim-cucumber'               " Cucumber syntax
Plug 'tpope/vim-haml'                   " Haml syntax
Plug 'vim-scripts/c.vim'                " C syntax and shortcuts

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
Plug 'vim-scripts/ReplaceWithSameIndentRegister'
Plug 'vim-scripts/ReplaceWithRegister' " Replace with contents of a register
Plug 'vim-scripts/matchit.zip'     " goto matching delimiter with %
Plug 'vim-scripts/visualrepeat'    " extends dot operator to visual mode

" Code snippets
Plug 'SirVer/ultisnips'   " snippets engine, integrates with YCM
Plug 'honza/vim-snippets' " textmate-style code snippets

" Git
Plug 'airblade/vim-gitgutter' " git diff in the gutter
Plug 'gregsexton/gitv'        " gitk port. requires fugitive
Plug 'tpope/vim-fugitive'     " side-by-side git blame with :Gblame

" Probationary
" Plug '0x0dea/vim-molasses'     " don't allow inefficient keystrokes
Plug 'Lokaltog/vim-easymotion' " jump without numbers

call plug#end()
