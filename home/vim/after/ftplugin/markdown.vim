" Enable spellchecking for Markdown
  " autocmd FileType markdown setlocal spell

" Automatically wrap at 80 characters for Markdown
  " autocmd BufRead,BufNewFile *.md setlocal textwidth=80

let g:AutoPairs = {'(':')', '[':']', '{':'}','"':'"', '`':'`', "'":"'", '*':'*', '**':'**'}
setlocal spell
setlocal textwidth=80
