" Test Runners
" ------------
nmap <silent> <leader>s :TestNearest<CR>
nmap <silent> <leader>S :TestFile<CR>
nmap <silent> <leader>a :TestSuite<CR>
nmap <silent> <leader>l :TestLast<CR>

let g:test#strategy = 'tslime'

" Tmux Runner
" -----------
" h for vertical split to the right
let g:VtrOrientation = "h"
let g:VtrPercentage  = 50

" v for horizontal split below
" let g:VtrOrientation = "v"
" let g:VtrPercentage  = 20

" Leader-key mappings:
let g:VtrUseVtrMaps = 1

"  Normal mode:
"  Mapping      |   Command
"  -----------------------------
"  <leader>or   |   VtrOpenRunner<cr>
"  <leader>sc   |   VtrSendCommandToRunner<cr>
"  -----------------------------
"  <leader>rr   |   VtrResizeRunner<cr>
"  <leader>ror  |   VtrReorientRunner<cr>
"  <leader>sl   |   VtrSendLineToRunner<cr>
"  <leader>kr   |   VtrKillRunner<cr>
"  <leader>fr   |   VtrFocusRunner<cr>
"  <leader>dr   |   VtrDetachRunner<cr>
"  <leader>ar   |   VtrReattachRunner<cr>
"  <leader>cr   |   VtrClearRunner<cr>
"  <leader>fc   |   VtrFlushCommand<cr>
"
"  Visual mode:
"  Mapping      |   Command
"  -----------------------------
"  <leader>sv   |   VtrSendSelectedToRunner<cr>

