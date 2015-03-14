" Vim Tmux Runner
" -----------
" h for vertical split to the right
let g:VtrOrientation = "h"
let g:VtrPercentage  = 50

" Play nice with semantic whitespace languages
let g:VtrStripLeadingWhitespace = 0
let g:VtrClearEmptyLines = 0
let g:VtrAppendNewline = 1

nnoremap <silent><leader>pry :VtrOpenRunner {'orientation': 'h', 'percentage': 50, 'cmd': 'pry'}<CR>

"  Normal mode:
"  Mapping      |   Command
"  -----------------------------
"  <leader>or   |   VtrOpenRunner<cr>
"  <leader>sc   |   VtrSendCommandToRunner<cr>

"  <leader>fr   |   VtrFocusRunner<cr>
"  -----------------------------
"  <leader>rr   |   VtrResizeRunner<cr>
"  <leader>ror  |   VtrReorientRunner<cr>
"  <leader>sl   |   VtrSendLineToRunner<cr>
"  <leader>kr   |   VtrKillRunner<cr>
"  <leader>dr   |   VtrDetachRunner<cr>
"  <leader>ar   |   VtrReattachRunner<cr>
"  <leader>cr   |   VtrClearRunner<cr>
"  <leader>fc   |   VtrFlushCommand<cr>
"
"  Visual mode:
"  Mapping      |   Command
"  -----------------------------
"  <leader>sv   |   VtrSendSelectedToRunner<cr>

nnoremap <silent><leader>or :VtrOpenRunner<CR>
nnoremap <silent><leader>sc :VtrSendCommandToRunner<CR>
nnoremap <silent><leader>fr :VtrFocusRunner<CR>
nnoremap <silent><leader>sl :VtrSendLineToRunner<CR>
nnoremap <silent><leader>kr :VtrKillRunner<CR>
nnoremap <silent><leader>dr :VtrDetachRunner<CR>
nnoremap <silent><leader>rar :VtrReattachRunner<CR>
nnoremap <silent><leader>cr :VtrClearRunner<CR>
nnoremap <silent><leader>fc :VtrFlushCommand<CR>
vnoremap <silent><leader>ss :VtrSendSelectedToRunner<CR>


" Test Runners
" ------------
nmap <silent> <leader>s :TestNearest<CR>
nmap <silent> <leader>S :TestFile<CR>
" nmap <silent> <leader>A :TestSuite<CR>
nmap <silent> <leader>l :TestLast<CR>

let g:test#strategy = 'tslime'

