" engage Git
nnoremap <Leader>g :Git<space>

" GitGutter settings and colors
let g:gitgutter_sign_column_always = 1
highlight clear SignColumn
highlight link GitGutterAdd SignColumn
highlight link GitGutterChange SignColumn
highlight link GitGutterDelete SignColumn
highlight link GitGutterChangeDelete SignColumn
highlight GitGutterAdd ctermfg=Green
highlight GitGutterChange ctermfg=Yellow
highlight GitGutterDelete ctermfg=Red
highlight GitGutterChangeDelete ctermfg=Red

" TODO: Add gvim colors, bar too
