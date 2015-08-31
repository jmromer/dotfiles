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

" vim-test
" -------------------------------------
nnoremap <silent><leader>s :TestNearest<CR>
nnoremap <silent><leader>S :TestFile<CR>
nnoremap <silent><leader>A :TestSuite<CR>
nnoremap <silent><leader>l :TestLast<CR>
nnoremap <silent><leader>g :TestVisit<CR>

" default test strategy is dispatch
let g:test#strategy = 'dispatch'

" Dispatch: Async test runner
" ----------------------------
nnoremap <leader>d :Dispatch<CR>
nnoremap <leader>D :Focus<SPACE>
nnoremap <leader>r :Start<SPACE>

" Vim Tmux Runner
" ----------------
" Keybindings
nnoremap <silent><leader>or :VtrOpenRunner<CR>
nnoremap <silent><leader>sc :w<CR>:VtrSendCommandToRunner!<CR>
nnoremap <silent><leader>fr :VtrFocusRunner<CR>
nnoremap <silent><leader>rl :w<CR>:VtrSendLineToRunner!<CR>
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
