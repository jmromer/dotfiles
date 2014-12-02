" -------------------- Syntax Highlighting ------------------------
syntax on    " turn syntax highlighting on
filetype on

" ------------- Syntastic (style checking) ------------------------
" configure syntastic syntax checking to check on open as well as save
let g:syntastic_check_on_open = 1
let g:syntastic_javascript_checkers = ['jslint']
let g:syntastic_ruby_checkers = ['rubocop']
let g:syntastic_aggregate_errors = 1
let g:syntastic_enable_signs=1
let g:syntastic_auto_jump=1
let g:syntastic_stl_format = '[%E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w}]'
let g:syntastic_html_tidy_ignore_errors=[" proprietary attribute \"ng-"]

" -------------------- Tab completion ------------------------
" Insert tab if at beginning of line, else use completion if
" not at beginning

set wildmode=list:longest,list:full

" function! InsertTabWrapper()
"   let col = col('.') - 1
"   if !col || getline('.')[col - 1] !~ '\k'
"     return "\<tab>"
"   else
"     return "\<c-p>"
"   endif
" endfunction
"
" inoremap <Tab> <c-r>=InsertTabWrapper()<cr>
" inoremap <S-Tab> <c-n>
"
