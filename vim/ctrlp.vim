" -------------------- Ctrl-P ------------------------
if executable('ag')
  " Prefer ag to grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

" summon CtrlP with <space>+f
let g:ctrlp_map = '<leader>f'

" Search by filename rather than full path by default
let g:ctrlp_by_filename = 1

" Search by Regexp as default
let g:ctrlp_regexp = 0

" Display CtrlP buffer at the top of the screen
let g:ctrlp_match_window = 'top,order:ttb,min:1,max:10,results:10'

" For already-opened files
" jump when <cr> is pressed, but only to windows in the current tab.
let g:ctrlp_switch_buffer = 'e'

" Set working path to the nearest ancestor of the current file that is a
" project root (contains .git, et al), else set to the current file's
" directory
let g:ctrlp_working_path_mode = 'rc'

" Add spec/ to the set of root markers
let g:ctrlp_root_markers = ['.git', 'app', 'lib', 'spec']

