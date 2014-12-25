" Test Runners
" ------------
nmap <silent> <leader>s :TestNearest<CR>
nmap <silent> <leader>S :TestFile<CR>
nmap <silent> <leader>a :TestSuite<CR>
nmap <silent> <leader>l :TestLast<CR>

let g:test#strategy = 'tslime'

