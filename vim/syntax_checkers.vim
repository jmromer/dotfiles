" ------------- Syntastic (style checking) ------------------------
" configure syntastic syntax checking to check on open as well as save
let g:syntastic_check_on_open = 1
let g:syntastic_aggregate_errors = 1
let g:syntastic_enable_signs=1

let g:syntastic_stl_format = '[%E{%e errors: %fe}%B{ }%W{%w warnings: %fw}]'
let g:syntastic_html_tidy_ignore_errors=[" proprietary attribute \"ng-"]

" jump to first issue detected, but only if it's an error
let g:syntastic_auto_jump=0
" don't perform checks when :wq :x or :ZZ are issued
let g:syntastic_check_on_wq=0


let g:syntastic_javascript_checkers = ['jslint']
let g:syntastic_javascript_jslint_args = ""

let g:syntastic_coffee_checkers = ['coffeelint']
let g:syntastic_coffee_coffeelint_args = "-f ~/.coffeelint.json"

let g:syntastic_ruby_checkers = ['rubocop']
let g:syntastic_coffee_coffeelint_args = ""

