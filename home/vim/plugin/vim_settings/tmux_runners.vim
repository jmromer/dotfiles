function! SwapTestRunner()
  if g:test#strategy == 'vtr'
    let g:test#strategy = 'dispatch'
    echo "using Dispatch"
  else
    let g:test#strategy = 'vtr'
    echo "using Vim-Tmux-Runner"
  endif
endfunction

nnoremap <leader>sr :call SwapTestRunner()<CR>

" Async Test Runner: Dispatch
" ----------------------------
nnoremap <leader>d :Dispatch<CR>
nnoremap <leader>D :Focus<SPACE>
nnoremap <leader>p :Start pry<CR>
nnoremap <leader>r :Start<SPACE>

" Tmux Test Runners: vim-test with VTR
" -------------------------------------
nmap <silent> <leader>s :TestNearest<CR>
nmap <silent> <leader>S :TestFile<CR>
nmap <silent> <leader>A :TestSuite<CR>
nmap <silent> <leader>l :TestLast<CR>
nmap <silent> <leader>g :TestVisit<CR>

let g:test#strategy = 'dispatch'

" Vim Tmux Runner
" ----------------
" Keybindings
nnoremap <silent><leader>pry :VtrOpenRunner {'orientation': 'h', 'percentage': 50, 'cmd': 'pry'}<CR>

nnoremap <silent><leader>or :VtrOpenRunner<CR>
nnoremap <silent><leader>sc :VtrSendCommandToRunner<CR>
nnoremap <silent><leader>fr :VtrFocusRunner<CR>
nnoremap <silent><leader>sl :VtrSendLineToRunner<CR>
nnoremap <silent><leader>kr :VtrKillRunner<CR>
nnoremap <silent><leader>dr :VtrDetachRunner<CR>
nnoremap <silent><leader>rar :VtrReattachRunner<CR>
nnoremap <silent><leader>cr :VtrClearRunner<CR>
nnoremap <silent><leader>fc :VtrFlushCommand<CR>
vnoremap <silent><leader>sv :VtrSendSelectedToRunner<CR>

" h for vertical split to the right
let g:VtrOrientation = "h"
let g:VtrPercentage  = 50

" Play nice with semantic whitespace languages
let g:VtrStripLeadingWhitespace = 0
let g:VtrClearEmptyLines = 0
let g:VtrAppendNewline = 1
