" ------------------- Plugin Settings ---------------------

" AutoPairs: disable closed-pair jumping instead of inserting
let g:AutoPairsFlyMode = 0
let g:AutoPairsShortcutBackInsert = '<C-b>'

" Blockle: Toggle ruby blocks with leader-tb
let g:blockle_mapping = '<leader>rtb'

" CoffeeScript: coffeescript syntax and helpers (CoffeeWatch, et al.)
let coffee_compile_vert = 1
let coffee_watch_vert = 1
let coffee_run_vert = 1

" Dragvisuals: keybindings
vmap <expr> <LEFT>   DVB_Drag('left')
vmap <expr> <RIGHT>  DVB_Drag('right')
vmap <expr> <DOWN>   DVB_Drag('down')
vmap <expr> <UP>     DVB_Drag('up')
vmap <expr> D        DVB_Duplicate()

" Dragvisuals: Remove any trailing whitespace introduced by move
let g:DVB_TrimWS = 1

" EasyAlign: Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
vmap <Enter> <Plug>(EasyAlign)

" EasyAlign: Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Easytags: Run asynchronously
let g:easytags_async = 1

" Easytags: use exuberant ctags
let g:easytags_cmd = '/usr/local/bin/ctags'

" Easytags: sacrifice some accuracy for performance
let g:easytags_syntax_keyword = 'always'

" Easytags: don't automatically update tags when pausing for a few seconds
let g:easytags_on_cursorhold = 0

" Easytags: don't automatically highlight tags after update
let g:easytags_auto_highlight = 0

" Easytags: update tags after save
let g:easytags_events = ['BufWritePost']

" Easytags: look for a project specific tags file
let g:easytags_dynamic_files = 1

" Easytags: language-specific settings
" JavaScript
let g:easytags_languages = {
\   'javascript': {
\       'cmd': 'jsctags',
\       'args': [],
\       'fileoutput_opt': '-f'
\   }
\}
" Golang
let g:easytags_languages = {
\   'go': {
\     'cmd': 'gotags',
\     'args': ['-sort=true'],
\     'fileoutput_opt': '-f=',
\     'stdout_opt': '-f=-',
\     'recurse_flag': '-R'
\   }
\}

" Haskell
if executable('hasktags')
  let g:easytags_languages = {
  \   'haskell': {
  \       'cmd': 'hasktags',
  \       'args': ['-x', '--ctags'],
  \       'fileoutput_opt': '-o',
  \       'stdout_opt': '-o-',
  \       'recurse_flag': '.'
  \   }
  \}
endif

" GitGutter: always show sign column
let g:gitgutter_sign_column_always = 1

" Molasses: prohibit inefficient repeated keystrokes
" the set of keys to monitor and punish against. default: 'hjkl'
let g:molasses_keys = 'hjkldu'

" the duration between acceptable presses of the same key. default: 200
let g:molasses_wait = 300

" HugeFile: disable options for large files (>= 1MB)
let g:hugefile_trigger_size = 1

" Netrw: file explorer
let g:netrw_liststyle = 4   " 4 lightweight, 3 tree
let g:netrw_preview   = 1   " open previews in vertical split (p)
let g:netrw_winsize   = 70  " give previewed windows 70% of screen width

" Togglecursor: insert mode uses an underline
let g:togglecursor_insert = 'underline'

" Vim Session: session autosave
let g:session_default_overwrite = 1
let g:session_autosave = 'no'

" YankRing: location of history file
let g:yankring_history_dir = '~/.vim/tmp'

" YankRing: Cycle through the yank register
let g:yankring_replace_n_pkey = '<C-p>'
let g:yankring_replace_n_nkey = '<C-n>'

" YankRing: display yankring contents
nnoremap <silent><leader>y :YRShow<CR>

