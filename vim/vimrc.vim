" ----------------- Base Settings ------------------------
set nocompatible        " use vim, not vi, settings.
set t_Co=256            " 256 color terminal
set background=dark
colorscheme Twilight

" if &diff
"   colorscheme xoria256
" endif

" --------------- Settings by Context ---------------------
runtime bundles.vim
runtime windows_and_nav.vim
runtime line_numbering.vim
runtime code_folding.vim
runtime syntax.vim
runtime autocommands.vim
runtime ctrlp.vim
runtime tabularize.vim
runtime airline.vim
runtime git.vim

" --------------- General Settings ---------------------
let mapleader = ' '   " use space as leader key

set history=50        " 50 items in command history
set ruler             " show the cursor position all the time
set showcmd           " display incomplete commands
set laststatus=2      " Always display the status line
set autowrite         " Automatically :write before running commands
set lazyredraw        " Only redraw for typed actions
set ttyfast           " This is a fast terminal
set grepprg=ag        " use ag for grepping

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
set autoindent     " automatically indent new lines
set shiftround     " always indent by multiple of shiftwidth
filetype plugin indent on

" Call out extra whitespace
set list listchars=tab:»·,trail:·,nbsp:·

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

" Always use vertical diffs
set diffopt+=vertical

" set esckeys  " will break any sequences using escape in insert mode
set timeoutlen=500 ttimeoutlen=10

" ------------------- Plugin Settings ---------------------
" vim-session: session autosave
let g:session_default_overwrite = 1
let g:session_autosave = 'no'

" netrw: file explorer
let g:netrw_liststyle=3  " thin (change to 3 for tree)
let g:netrw_banner=1     " no banner
let g:netrw_altv=1       " open files on right
let g:netrw_preview=1    " open previews vertically

" blockle.vim: Toggle ruby blocks with leader-tb
let g:blockle_mapping = '<Leader>rtb'

" vim-rspec: run specs in last session of current terminal
let g:rspec_runner = "os_x_iterm"

" togglecursor: insert mode uses an underline
let g:togglecursor_insert = 'underline'

" Easytags: Run asynchronously
let g:easytags_async = 1

" YankRing: location of history file
let g:yankring_history_dir = '~/.vim/tmp'

" YankRing: Free up Ctrl+p, Ctrl+n
let g:yankring_replace_n_pkey = '<m-p>'
let g:yankring_replace_n_nkey = '<m-n>'

" YouCompleteMe: use system python
let g:ycm_path_to_python_interpreter = '/usr/bin/python'

" YouCompleteMe: move through completion list
let g:ycm_key_list_select_completion = ['<Down>']

" YouCompleteMe: semantic completion trigger
let g:ycm_key_invoke_completion = '<C-k>'
" TODO: should this be invoked automatically? investigate.

" UltiSnips: Trigger configuration. Do not use <tab> if you use YCM
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsEditSplit="vertical"   " split window to edit snippet

" Multiple Cursors: use ctrl+m to enter multicursor mode
let g:multi_cursor_use_default_mapping=0
let g:multi_cursor_start_key='<C-m>'

" NERDTree: Show hidden files by default, replace netrw within panes
let NERDTreeShowHidden=1
let NERDTreeHijackNetrw=1

" Let Ctrl+P open files in NerdTree's initial buffer
let g:ctrlp_dont_split = 'NERD'

" ------------------- Leader key mappings ---------------------
" run commands in an interactive shell
nnoremap <Leader>e :RunInInteractiveShell<space>
" turn off highlighting by pressing enter
nnoremap <silent><CR> :noh<CR><CR>
" reload vimrc
nnoremap <silent><Leader>r :source ~/.vimrc<CR>:echoe "vim reloaded."<CR>
" redraw screen
nnoremap <silent><Leader>rr :redraw!<CR>
" toggle Tagbar
nnoremap <silent><leader>t :TagbarToggle<CR>
" toggle EasyBuffer
nnoremap <silent><leader>b :EasyBuffer<CR>
" toggle Gundo
nnoremap <silent><leader>u :GundoToggle<CR>
" kill trailing whitespace
nnoremap <silent><Leader>kw :EraseBadWhitespace<CR>
" open netrw explore buffer
nnoremap <silent><C-e> :Explore<CR>
" toggle NERDTree pane
nnoremap <silent><Leader>n :NERDTreeToggle<CR>
" see YankRing contents
nnoremap <silent><Leader>v :YRShow<CR>

" Test Runners
" ------------
" rspec: run nearest spec
nmap <Leader>s :call RunNearestSpec()<CR>
" rspec: run the last spec run
nmap <Leader>sl :call RunLastSpec()<CR>
" rspec: run current spec file
nmap <Leader>sf :call RunCurrentSpecFile()<CR>
" rspec: run all specs
nmap <Leader>sa :call RunAllSpecs()<CR>
" cucumber: run current scenario
map <Leader>c :w<cr>:exe "!cucumber %" . ":" . line(".")<cr>
" cucumber: run current feature file
map <Leader>cf :w<cr>:!cucumber %<cr>
" cucumber: run all feature files
map <Leader>ca :w<cr>:!cucumber<cr>

" Session Management
" ------------------
nnoremap <Leader>ss :SaveSession<space>
nnoremap <Leader>os :OpenSession<space>

" Text search
" ------------
" Grep for the word under the cursor, open results in quickfix pane
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

" Grep with given arguments, open results in quickfix pane
command! -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
nnoremap <Leader>\ :Ag<SPACE>

" ----------------------- Mappings: Copy/Paste ------------------------
" Select all text in file
nmap <Leader>a ggVG
" Copy to system clipboard
vmap <Leader>c "*y
" Cut to system clipboard
vmap <Leader>x "*d
" Paste from system clipboard
map <silent><Leader>p :set paste<CR>i<ESC>"+p:set nopaste<CR>

