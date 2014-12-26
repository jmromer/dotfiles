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

" Easytags: Run asynchronously
let g:easytags_async = 1

" YankRing: location of history file
let g:yankring_history_dir = '~/.vim/tmp'

" YankRing: Free up Ctrl+p, Ctrl+n
let g:yankring_replace_n_pkey = '<M-p>'
let g:yankring_replace_n_nkey = '<M-n>'

" YouCompleteMe: use system python
let g:ycm_path_to_python_interpreter = '/usr/bin/python'

" YouCompleteMe: move through completion list
let g:ycm_key_list_select_completion = ['<C-n>']
let g:ycm_key_list_previous_completion = ['<C-p>']

" YouCompleteMe: semantic completion trigger
let g:ycm_key_invoke_completion = '<C-Space>'

" UltiSnips: Trigger configuration.
" Do not use <tab> if you use YCM -- you'll expand snippets prematurely
let g:UltiSnipsExpandTrigger = "<CR>"
let g:UltiSnipsJumpForwardTrigger = "<Tab>"
let g:UltiSnipsJumpBackwardTrigger = "<S-Tab>"
let g:UltiSnipsEditSplit = "vertical"   " split window to edit snippet

