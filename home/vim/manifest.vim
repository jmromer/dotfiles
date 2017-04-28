" ---------------- Plugins --------------------------
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }

function! DoRemote(arg)
  UpdateRemotePlugins
endfunction

if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': function('DoRemote') }
else
  Plug 'Valloric/YouCompleteMe', { 'dir': '~/.vim/plugged/YouCompleteMe', 'do': './install.py --clang-completer --gocode-completer --tern-completer' }
endif

Plug 'benekastah/neomake'              " async linting, et al. best with nvim
Plug 'kien/rainbow_parentheses.vim'    " delimiter highlighting
Plug 'ntpeters/vim-better-whitespace'  " detect and highlight bad whitespace
Plug 'tpope/vim-surround'              " surround text block with delimiters
Plug 'tomtom/tcomment_vim'             " gcc / gc[ai][pim]
Plug 'christoomey/vim-system-copy'     " cp
Plug 'christoomey/vim-sort-motion'     " gs[ai][p({]
Plug 'itchyny/lightline.vim'           " lightweight, configurable status line
Plug 'SirVer/ultisnips'                " snippets engine, integrates with YCM
Plug 'vim-scripts/YankRing.vim'        " maintain yank/del history
Plug 't9md/vim-ruby-xmpfilter', { 'for': 'ruby' }
Plug 'tpope/vim-rails', { 'for': ['ruby']  }
Plug 'tpope/vim-vinegar'              " enhancements to netrw
Plug 'tpope/vim-git'                  " git commit filetype settings, et al
Plug 'mattn/emmet-vim', { 'for': ['html', 'eruby', 'css', 'scss'] }

" Plug 'unblevable/quick-scope'          " highlight chars for jumping
" Plug 'chiel92/vim-autoformat'         " auto-format with supported formatters
" Plug 'dkprice/vim-easygrep'           " global search-and-replace
" Plug 'jiangmiao/auto-pairs'           " automatic delimiter closing
" Plug 'jszakmeister/vim-togglecursor'  " toggle cursor between modes
" Plug 'junegunn/vim-peekaboo'          " register preview
" Plug 'mhinz/vim-hugefile'             " disable options for huge files
" Plug 'pbrisbin/vim-mkdir'             " create subdirectories as needed
" Plug 'rizzatti/dash.vim'              " search Dash documentation
" Plug 'tpope/vim-eunuch'               " Unix commands in vim
" Plug 'tpope/vim-rhubarb'              " GitHub extension for fugitive.vim
" Plug 'tpope/vim-rsi'                  " smart readline bindings in cmode et al
" Plug 'tpope/vim-sleuth'               " detect indentation level
" Plug 'vim-scripts/netrw.vim'          " older (bug-free) version of netrw
" Plug 'vitorgalvao/autoswap_mac'       " auto-respond to swap file message
" Plug 'wesQ3/vim-windowswap'           " move panes around with <leader>ww
" Plug 'xolox/vim-easytags'             " automatically updated ctags
" Plug 'xolox/vim-misc' | Plug 'xolox/vim-session' " session management

" Speedy text editing
" Plug 'junegunn/vim-easy-align'   " text alignment
" Plug 'tpope/vim-unimpaired'      " useful complementary mappings

" " Tmux, Tmux runners
" Plug 'tpope/vim-tbone'             " basic tmux support
" Plug 'benmills/vimux'              " interact with tmux from vim
" Plug 'janko-m/vim-test'            " runner for rspec, cucumber, et al.
"
" " Display conveniences
" Plug 'Yggdroot/indentLine' " display indentation guides with :IndentLineToggle
"
" " -------- Language-Specific -----------
" Plug 'elixir-lang/vim-elixir', { 'for': 'elixir' }
" Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
" Plug 'lervag/vim-latex', { 'for': 'latex' }
" Plug 'tpope/vim-haml', { 'for': 'haml' }
" Plug 'kballard/vim-swift', { 'for': 'swift' }
" Plug 'fatih/vim-go', { 'for': 'go' }

" " Scala
" Plug 'derekwyatt/vim-scala', { 'for': ['scala', 'sbt.scala'] }
" Plug 'ktvoelker/sbt-vim',    { 'for': ['scala', 'sbt.scala'] }
"
" " Ruby
" Plug 'ck3g/vim-change-hash-syntax', { 'for': 'ruby' }
" Plug 'ecomba/vim-ruby-refactoring', { 'for': 'ruby' }
" Plug 'nelstrom/vim-textobj-rubyblock', { 'for': 'ruby' }
" Plug 'tpope/vim-endwise', { 'for': 'ruby' }
" Plug 'tpope/vim-rbenv', { 'for': 'ruby' }
" Plug 'noprompt/vim-yardoc', { 'for': 'ruby' }
" Plug 'vim-ruby/vim-ruby' " can't be lazy-loaded with { 'for': 'ruby' }
" Plug 'tpope/vim-bundler'
" Plug 'tpope/vim-projectionist' | Plug 'tpope/vim-rake'
" Plug 'slim-template/vim-slim', { 'for': 'slim' }
"
" " JavaScript
" Plug 'marijnh/tern_for_vim', { 'for': 'javascript', 'do': 'npm install' }
" Plug 'jelera/vim-javascript-syntax', { 'for': 'javascript' }
" Plug 'moll/vim-node', { 'for': 'javascript' }
" Plug 'mxw/vim-jsx', { 'for': 'javascript.jsx' }
" Plug 'othree/javascript-libraries-syntax.vim', { 'for': 'javascript' }
" Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'javascript.jsx'] }
" Plug 'dsawardekar/portkey' | Plug 'dsawardekar/ember.vim'
"
" " OCaml
" Plug 'the-lambda-church/merlin'
" Plug 'OCamlPro/ocp-indent'
" Plug 'def-lkb/ocp-indent-vim'
"
" " Elm
" Plug 'ElmCast/elm-vim'
"
" " Clojure
" Plug 'tpope/vim-sexp-mappings-for-regular-people', { 'for': 'clojure'}
" Plug 'tpope/vim-fireplace', { 'for': 'clojure'}
"
" " -------- Configure -----------

" -------- Probationary -----------
