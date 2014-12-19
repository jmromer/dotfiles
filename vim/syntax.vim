" -------------------- Syntax Highlighting ------------------------
syntax on    " turn syntax highlighting on
filetype on

" ------------- Syntastic (style checking) ------------------------
" configure syntastic syntax checking to check on open as well as save
let g:syntastic_check_on_open = 1
let g:syntastic_aggregate_errors = 1
let g:syntastic_enable_signs=1

let g:syntastic_stl_format = '[%E{%e errors: %fe}%B{ }%W{%w warnings: %fw}]'
let g:syntastic_html_tidy_ignore_errors=[" proprietary attribute \"ng-"]

" jump to first issue detected, but only if it's an error
let g:syntastic_auto_jump=2
" don't perform checks when :wq :x or :ZZ are issued
let g:syntastic_check_on_wq = 0


let g:syntastic_javascript_checkers = ['jslint']
let g:syntastic_javascript_jslint_args = ""

let g:syntastic_ruby_checkers = ['rubocop']
let g:syntastic_ruby_rubocop_args = ""

" -------------------- Tab completion ------------------------
" Insert tab if at beginning of line, else use completion if
" not at beginning
"
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
