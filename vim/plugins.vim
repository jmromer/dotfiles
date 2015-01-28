" ------------------- Plugin Settings ---------------------
" vim-session: session autosave
let g:session_default_overwrite = 1
let g:session_autosave = 'no'

" netrw: file explorer
let g:netrw_liststyle = 3   " 1 thin, 3 tree
let g:netrw_preview   = 1   " open previews in vertical split (p)
let g:netrw_winsize   = 70  " give previewed windows 70% of screen width

" Fixes clang 'pattern not found' messages
let g:clang_user_options='|| exit 0'

" blockle.vim: Toggle ruby blocks with leader-tb
let g:blockle_mapping = '<Leader>rtb'

" togglecursor: insert mode uses an underline
let g:togglecursor_insert = 'underline'

" dragvisuals.vim: keybindings
vmap <expr> <LEFT>   DVB_Drag('left')
vmap <expr> <RIGHT>  DVB_Drag('right')
vmap <expr> <DOWN>   DVB_Drag('down')
vmap <expr> <UP>     DVB_Drag('up')
vmap <expr> D        DVB_Duplicate()

let g:DVB_TrimWS = 1 " Remove any trailing whitespace introduced by move

" Easytags: Run asynchronously
let g:easytags_async = 1

" YankRing: location of history file
let g:yankring_history_dir = '~/.vim/tmp'

" YankRing: Cycle through the yank register
let g:yankring_replace_n_pkey = '<C-p>'
let g:yankring_replace_n_nkey = '<C-n>'

" YankRing: display yankring contents
nnoremap <silent><Leader>y :YRShow<CR>

" GitGutter: always show sign column
let g:gitgutter_sign_column_always = 1

" Airline: General settings
let g:airline#extensions#tabline#enabled = 1  " Smarter tab line
let g:airline_powerline_fonts=1               " Use powerline symbols
let g:airline#extensions#tmuxline#enabled = 0 " Don't use tmux bar
let g:airline_theme = 'solarized'             " alt: simple
let g:airline#extensions#hunks#enabled = 0    " show branch name, not changes

