" Disable ctags version warning (since universal-ctags)
let g:easytags_suppress_ctags_warning = 1

" Easytags: Run asynchronously
let g:easytags_async = 1

" Easytags: use exuberant / universal ctags
let g:easytags_cmd = '/usr/local/bin/ctags'

" Easytags: look for a project specific tags file
set tags=./.git/tags;
let g:easytags_dynamic_files = 1


" ----- Easytags: language-specific settings ------

" JavaScript
let g:easytags_languages = {
\   'javascript': {
\       'cmd': 'jsctags',
\       'args': [],
\       'fileoutput_opt': '-f'
\   },
\   'go': {
\     'cmd': 'gotags',
\     'args': ['-sort=true'],
\     'fileoutput_opt': '-f=',
\     'stdout_opt': '-f=-',
\     'recurse_flag': '-R'
\   },
\   'haskell': {
\       'cmd': 'hasktags',
\       'args': ['-x', '--ctags'],
\       'fileoutput_opt': '-o',
\       'stdout_opt': '-o-',
\       'recurse_flag': '.'
\   }
\}
