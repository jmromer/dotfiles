if has('nvim')
  autocmd! BufWritePost * Neomake
endif

let g:neomake_coffee_enabled_makers     = ['coffeelint']
let g:neomake_css_enabled_makers        = ['csslint']
let g:neomake_elixir_enabled_makers     = ['credo']
let g:neomake_go_enabled_makers         = ['golint']
let g:neomake_haml_enabled_makers       = ['haml-lint']
let g:neomake_javascript_enabled_makers = ['eslint']
let g:neomake_json_enabled_makers       = ['jsonlint']
let g:neomake_jsx_enabled_makers        = ['eslint']
let g:neomake_markdown_enabled_makers   = ['mdl']
let g:neomake_python_enabled_makers     = ['pep8']
let g:neomake_ruby_enabled_makers       = ['rubocop']
let g:neomake_sh_enabled_makers         = ['shellcheck']
let g:neomake_vim_enabled_makers        = ['vint']
let g:neomake_yaml_enabled_makers       = ['yamllint']
let g:neomake_haml_enabled_makers       = ['hamllint']
