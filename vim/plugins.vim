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
