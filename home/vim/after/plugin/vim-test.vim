" vim-test
" (loaded /after because it doesn't work in the normal place, becasue reasons)
" ----------------------------------------------------------------------------
nnoremap <leader>s :TestNearest<CR>
nnoremap <leader>S :TestFile<CR>
nnoremap <leader>A :TestSuite<CR>
nnoremap <leader>l :TestLast<CR>
nnoremap <leader>g :TestVisit<CR>

" default test strategy is vimux
let g:test#strategy = 'vimux'
