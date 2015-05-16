function! SwapTestRunner()
  if g:test#strategy == 'vtr'
    let g:test#strategy = 'dispatch'
    echo "using Dispatch"
  else
    let g:test#strategy = 'vtr'
    echo "using Vim-Tmux-Runner"
  endif
endfunction

function! OpenPry()
  if g:test#strategy == 'dispatch'
    :Start pry
  else
    :VtrOpenRunner {'orientation': 'h', 'percentage': 50, 'cmd': 'pry'}
  end
endfunction

nnoremap <leader>sr :call SwapTestRunner()<CR>
nnoremap <leader>p :call OpenPry()<CR>

" default test strategy is dispatch
let g:test#strategy = 'dispatch'

" Async Test Runner: Dispatch
" ----------------------------
nnoremap <leader>d :Dispatch<CR>
nnoremap <leader>D :Focus<SPACE>
nnoremap <leader>r :Start<SPACE>

" Tmux Test Runners: vim-test with VTR
" -------------------------------------
nmap <silent> <leader>s :TestNearest<CR>
nmap <silent> <leader>S :TestFile<CR>
nmap <silent> <leader>A :TestSuite<CR>
nmap <silent> <leader>l :TestLast<CR>
nmap <silent> <leader>g :TestVisit<CR>

" Vim Tmux Runner
" ----------------
" Keybindings
nnoremap <silent><leader>or :VtrOpenRunner<CR>
nnoremap <silent><leader>rc :VtrSendCommandToRunner!<CR>
nnoremap <silent><leader>fr :VtrFocusRunner<CR>
nnoremap <silent><leader>rl :VtrSendLineToRunner!<CR>
nnoremap <silent><leader>kr :VtrKillRunner<CR>
nnoremap <silent><leader>dr :VtrDetachRunner<CR>
nnoremap <silent><leader>ar :VtrReattachRunner<CR>
nnoremap <silent><leader>cr :VtrClearRunner<CR>
nnoremap <silent><leader>fc :VtrFlushCommand<CR>
vnoremap <silent><leader>sv :VtrSendSelectedToRunner!<CR>

" h for vertical split to the right
" v for horizontal split below
let g:VtrOrientation = "v"
let g:VtrPercentage  = 10

" Play nice with semantic whitespace languages
let g:VtrStripLeadingWhitespace = 0
let g:VtrClearEmptyLines = 0
let g:VtrAppendNewline = 1
