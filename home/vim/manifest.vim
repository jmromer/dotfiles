" ---------------- Plugins --------------------------
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }
Plug 'Valloric/YouCompleteMe', { 'dir': '~/.vim/plugged/YouCompleteMe', 'do': './install.py --clang-completer --gocode-completer --tern-completer' }

Plug 'benekastah/neomake'              " async linting, et al. best with nvim
Plug 'kien/rainbow_parentheses.vim'    " delimiter highlighting
Plug 'ntpeters/vim-better-whitespace'  " detect and highlight bad whitespace
Plug 'tpope/vim-surround'              " surround text block with delimiters
Plug 'tomtom/tcomment_vim'             " gcc / gc[ai][pim]
Plug 'christoomey/vim-system-copy'     " cp
Plug 'itchyny/lightline.vim'           " lightweight, configurable status line
Plug 'SirVer/ultisnips'                " snippets engine, integrates with YCM
Plug 'vim-scripts/YankRing.vim'        " maintain yank/del history
Plug 't9md/vim-ruby-xmpfilter', { 'for': 'ruby' }
Plug 'tpope/vim-rails', { 'for': ['ruby']  }
Plug 'tpope/vim-vinegar'              " enhancements to netrw
Plug 'tpope/vim-git'                  " git commit filetype settings, et al
Plug 'mattn/emmet-vim', { 'for': ['html', 'eruby', 'css', 'scss'] }

Plug 'jszakmeister/vim-togglecursor'  " toggle cursor between modes
Plug 'pbrisbin/vim-mkdir'             " create subdirectories as needed
