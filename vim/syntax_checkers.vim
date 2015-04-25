" ------------- Syntastic (style checking) ------------------------
" configure syntastic syntax checking to check on open as well as save
let g:syntastic_check_on_open = 1
let g:syntastic_aggregate_errors = 1
let g:syntastic_enable_signs = 1

" automatically load errors into the location list
let g:syntastic_always_populate_loc_list = 1

" display errors and warnings
let g:syntastic_stl_format = '[%E{%eE%fe}%B{ }%W{%wW%fw}]'

" don't jump to first error or warning
let g:syntastic_auto_jump = 0

" don't perform checks when :wq :x or :ZZ are issued
let g:syntastic_check_on_wq = 0

" don't show html/tidy errors for handlebars/mustache templates
let g:syntastic_filetype_map = { 'html.handlebars': 'handlebars' }

" manually call :SyntasticCheck for html files
let syntastic_mode_map = { 'passive_filetypes': ['html']  }


" JavaScript
let g:syntastic_javascript_checkers = ['jshint', 'jscs']
let g:syntastic_javascript_jslint_args = ''
let g:syntastic_javascript_jshint_args = ''
let g:syntastic_javascript_jscsrc = '-c ~/.jscsrc'

" CoffeeScript
let g:syntastic_coffee_checkers = ['coffeelint']
let g:syntastic_coffee_coffeelint_args = '-f ~/.coffeelint.json'

" Ruby
let g:syntastic_ruby_checkers = ['rubocop']
let g:syntastic_ruby_rubocop_args = ''

