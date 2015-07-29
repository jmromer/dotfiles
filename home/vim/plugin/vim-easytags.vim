" Easytags: Run asynchronously
let g:easytags_async = 1

" Easytags: use exuberant ctags
let g:easytags_cmd = '/usr/local/bin/ctags'

" Easytags: look for a project specific tags file
set tags=./tags;
let g:easytags_dynamic_files = 1


" ----- Easytags: language-specific settings ------

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
