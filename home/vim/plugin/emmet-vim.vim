
" Disable by default
let g:user_emmet_install_global = 0

"enable for html, css, erb, scss
augroup emmetVim
  autocmd!
  autocmd FileType html,css,eruby,scss EmmetInstall
augroup END

" let g:user_emmet_leader_key='<C-y>'
