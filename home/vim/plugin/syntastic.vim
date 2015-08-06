" ------------- Syntastic (style checking) ------------------------
" check only on save
let g:syntastic_check_on_open = 0
let g:syntastic_aggregate_errors = 1
let g:syntastic_enable_signs = 1

" automatically load errors into the location list
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0

" Better :sign interface symbols
let g:syntastic_warning_symbol = '✘'
let g:syntastic_error_symbol   = '✘'
let g:syntastic_style_warning_symbol = '•'
let g:syntastic_style_error_symbol = '•'

" " Syntastic Highlighting defined in theme file. Kept here for documentation.
" " Use yellow for warnings, red for errors
" highlight! SyntasticWarningSign      ctermfg=136 ctermbg=none
" highlight! SyntasticStyleWarningSign ctermfg=136 ctermbg=none
" highlight! SyntasticErrorSign        ctermfg=160 ctermbg=none
" highlight! SyntasticStyleErrorSign   ctermfg=160 ctermbg=none
" highlight! link SyntasticError SpellBad
" highlight! link SyntasticWarning SpellCap

" display errors and warnings
let g:syntastic_stl_format = '[%E{%eE%fe}%B{ }%W{%wW%fw}]'

" don't jump to first error or warning
let g:syntastic_auto_jump = 0

" don't perform checks when :wq :x or :ZZ are issued
let g:syntastic_check_on_wq = 0

" lint handlebars templaates with handlebars
let g:syntastic_filetype_map = { 'html.handlebars': 'handlebars' }

" don't bother linting html
let g:syntastic_html_checkers = []


" JavaScript
let g:syntastic_javascript_checkers = ['eslint', 'jscs']
let g:syntastic_javascript_eslint_args = ''
let g:syntastic_javascript_jscsrc = ''

" CoffeeScript
let g:syntastic_coffee_checkers = ['coffeelint']
let g:syntastic_coffee_coffeelint_args = '-f ~/.coffeelint.json'

" Ruby
let g:syntastic_ruby_checkers = ['rubocop']
let g:syntastic_ruby_rubocop_args = ''
