" vim-test
" -------------------------------------
nnoremap <silent><leader>s :TestNearest<CR>:VtrFocusRunner<CR>
nnoremap <silent><leader>S :TestFile<CR>:VtrFocusRunner<CR>
nnoremap <silent><leader>A :TestSuite<CR>
nnoremap <silent><leader>l :TestLast<CR>
nnoremap <silent><leader>g :TestVisit<CR>

" default test strategy is vim-tmux-runner
let g:test#strategy = 'vtr'
