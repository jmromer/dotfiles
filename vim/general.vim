scriptencoding utf-8
set encoding=utf-8

" --------------- General Settings ---------------------
let mapleader = ' '   " use space as leader key

set history=50        " 50 items in command history
set ruler             " show the cursor position all the time
set showcmd           " display incomplete commands
set laststatus=2      " Always display the status line
set autowrite         " Automatically :write before running commands
set lazyredraw        " Redraw for typed actions, not when executing macros
set ttyfast           " This is a fast terminal
set grepprg=ag        " use ag for grepping
set noshowmode        " don't show the current mode in the message bar

" Backup policy
set backup                   " keep backup files
set backupdir=~/.vim/backup/ " store backup files in ~/.vim/backup
set directory=~/.vim/tmp/    " store swap files in ~/.vim/tmp
set backupskip=/tmp/*,/private/tmp/*
set writebackup

" Backspace behavior
set backspace=indent,start " allow backspace over indent and start, not eol.

" Searching
set incsearch     " do incremental searching
set hlsearch      " highlight search matches by default
set ignorecase    " case insensitive pattern matching
set smartcase     " overrides ignorecase if pattern contains upcased chars
let @/ = ""       " clear the search register
:nohlsearch       " clear any previously highlighted search matches

" Indentation with soft tabs, 2 spaces
set expandtab      " use soft tabs
set shiftwidth=2   " spaces per tab (when shifting)
set softtabstop=2  " 2-space soft tabs
set shiftround     " always indent by multiple of shiftwidth

" Disabled indent settings (override syntax-based indentation)
" set autoindent     " automatically indent new lines
" set smartindent    " automatically indent after certain characters

" Call out extra whitespace
set list listchars=tab:»·,trail:·,nbsp:·

" Command mode autocompletion settings
set wildmode=list:longest,list:full

" Make message bar taller to avoid 'press enter' prompt
set shortmess=a
set cmdheight=2

" Window size shifts on focus, current screen stays larger
set winwidth=84
set winheight=5
set winminheight=5
set winheight=999

" Delimiter matching
set showmatch      " Show matching brackets.
set matchtime=5    " Bracket blinking.

" Make it obvious where 80 characters is
set textwidth=80
set colorcolumn=+0

" keep context lines you would like to see above and below the cursor
set scrolloff=10

" Always use vertical diffs
set diffopt+=vertical

" set esckeys  " will break any sequences using escape in insert mode
set timeoutlen=500 ttimeoutlen=10

" Remap increment and decrement
nnoremap + <C-a>
nnoremap - <C-x>

" Keep cursor in place when joining lines
nnoremap J mzJ`z

" Preserve words when breaking lines
set linebreak

" Make Y consistent with D. Use yy to yank an entire line, like dd
nnoremap Y y$

