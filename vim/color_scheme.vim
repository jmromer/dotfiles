" Base vim settings
syntax on     " enable syntax highlighting
set t_Co=256  " 256 color terminal

" Allow italic text in vim
set t_ZH=[3m
set t_ZR=[23m

" Color scheme settings
set background=dark
let g:solarized_termcolors=16
colorscheme solarized  " Twilight

" ------------- Theme Development -----------------------
" Show syntax highlighting groups for word under cursor
" nmap <Leader>p :call <SID>SynStack()<CR>
" function! <SID>SynStack()
"   if !exists("*synstack")
"     return
"   endif
"   echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
" endfunc

