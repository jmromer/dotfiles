" ------------- Syntastic (style checking) ------------------------
" configure syntastic syntax checking to check on open as well as save
let g:syntastic_check_on_open = 1
let g:syntastic_aggregate_errors = 1
let g:syntastic_enable_signs = 1
"
" automatically load errors into the location list
let g:syntastic_always_populate_loc_list = 1

let g:syntastic_stl_format = '[%E{%e E%fe}%B{ }%W{%w W%fw}]'
let g:syntastic_html_tidy_ignore_errors = [" proprietary attribute \"ng-"]

" don't jump to first error or warning
let g:syntastic_auto_jump = 0

" don't perform checks when :wq :x or :ZZ are issued
let g:syntastic_check_on_wq = 0


let g:syntastic_javascript_checkers = ['jshint', 'jscs']
let g:syntastic_javascript_jslint_args = ""
let g:syntastic_javascript_jshint_args = ""
let g:syntastic_javascript_jscsrc = "-c ~/.jscsrc"

let g:syntastic_coffee_checkers = ['coffeelint']
let g:syntastic_coffee_coffeelint_args = "-f ~/.coffeelint.json"

let g:syntastic_ruby_checkers = ['rubocop']
let g:syntastic_coffee_coffeelint_args = ""

" don't show html/tidy errors for handlebars/mustache templates
let g:syntastic_filetype_map = { 'html.handlebars': 'handlebars' }
