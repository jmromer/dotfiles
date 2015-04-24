" -------------------- Ctrl-P ------------------------
if executable('ag')
  " Not for CtrlP: Use Ag to search for files
  set grepprg=ag\ --nogroup\ --nocolor\ -g

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -i -l --nocolor --nogroup -g ""'
  " Use caching, even though ag is fast
  let g:ctrlp_use_caching = 0
endif

" Use PyMatcher as CtrlP's matching function
let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }

" summon CtrlP with <space>+f
let g:ctrlp_map = '<leader>f'

" Search by filename rather than full path by default
let g:ctrlp_by_filename = 1

" Search by Regexp as default
let g:ctrlp_regexp = 0

" CtrlP display preferences
let g:ctrlp_match_window = 'bottom,order:ttb,min:1,max:10,results:10'

" For already-opened files
" jump when <cr> is pressed, but only to windows in the current tab.
let g:ctrlp_switch_buffer = 'e'

" Set working path to the nearest ancestor of the current file that is a
" project root (contains .git, et al), else set to the current file's
" directory
let g:ctrlp_working_path_mode = 'rc'

" Add spec/ to the set of root markers
let g:ctrlp_root_markers = ['.git', 'spec']

" Ignore some folders and files for CtrlP indexing
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\.git$\|\.yardoc\|public$|log\|tmp$',
  \ 'file': '\.so$\|\.dat$|\.DS_Store$'
  \ }

