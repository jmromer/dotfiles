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
