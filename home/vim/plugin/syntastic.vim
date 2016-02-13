" ------------- Syntastic (style checking) ------------------------
" checking on open slows navigation too much.
let g:syntastic_check_on_open = 0

" check only on demand
let g:syntastic_mode_map = {
    \ "mode": "passive",
    \ "active_filetypes": [],
    \ "passive_filetypes": []
    \ }

" don't perform checks when :wq :x or :ZZ are issued
let g:syntastic_check_on_wq = 0

" let g:syntastic_aggregate_errors = 1
let g:syntastic_enable_signs = 1

" automatically load errors into the location list
let g:syntastic_always_populate_loc_list = 1

" Better :sign interface symbols
let g:syntastic_warning_symbol = '•'
let g:syntastic_style_warning_symbol = '•'

let g:syntastic_error_symbol   = '✘'
let g:syntastic_style_error_symbol = '✘'

" " Syntastic Highlighting defined in theme file. Kept here for documentation.
" " Use yellow for style checks, red for syntax
" highlight! SyntasticWarningSign      ctermfg=160 ctermbg=none
" highlight! SyntasticErrorSign        ctermfg=160 ctermbg=none
"
" highlight! SyntasticStyleWarningSign ctermfg=172 ctermbg=none
" highlight! SyntasticStyleErrorSign   ctermfg=172 ctermbg=none
"
" highlight! link SyntasticError SpellBad
" highlight! link SyntasticWarning SpellCap

" display errors and warnings
let g:syntastic_stl_format = '[%E{%eE%fe}%B{ }%W{%wW%fw}]'

" do not jump to first error or warning
let g:syntastic_auto_jump = 0

" lint handlebars templaates with handlebars
let g:syntastic_filetype_map = { 'html.handlebars': 'handlebars' }

" JavaScript
let g:syntastic_javascript_checkers = ['eslint', 'jscs']
let g:syntastic_javascript_eslint_args = ''
let g:syntastic_javascript_jscsrc = ''

" CoffeeScript
let g:syntastic_coffee_checkers = ['coffeelint']
let g:syntastic_coffee_coffeelint_args = '-f ~/.coffeelint.json'

" Ruby
let g:syntastic_ruby_checkers = ['rubocop', 'mri']
let g:syntastic_ruby_rubocop_args = ''

" OCaml
let g:syntastic_ocaml_checkers = ['merlin', 'camlp4o']

" Ignore spurious warnings
let g:syntastic_html_tidy_ignore_errors=[" proprietary attribute \"ng-"]
let g:syntastic_eruby_ruby_quiet_messages =
    \ {"regex": "possibly useless use of a variable in void context"}
