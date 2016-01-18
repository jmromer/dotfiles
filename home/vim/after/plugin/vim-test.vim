" vim-test
" (loaded /after because it doesn't work in the normal place,
" because reasons)
" ----------------------------------------------------------------------------
nnoremap <leader>s :w<CR>:TestNearest<CR>
nnoremap <leader>S :w<CR>:TestFile<CR>
nnoremap <leader>A :wa<CR>:TestSuite<CR>
nnoremap <leader>l :w<CR>:TestLast<CR>
nnoremap <leader>g :w<CR>:TestVisit<CR>

" default test strategy is vimux
let g:test#strategy = 'vimux'
