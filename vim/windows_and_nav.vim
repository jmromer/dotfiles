" -------------- Navigation / Window Management ------------------------ "
" shift-h / shift-l to switch tabs
nnoremap <S-h> gT
nnoremap <S-l> gt

" Use ctrl+(j|k|l|i) to move between panes
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" Disable directional keys
noremap <Left>  :echoe "Use h"<CR>
noremap <Right> :echoe "Use l"<CR>
noremap <Up>    :echoe "Use k"<CR>
noremap <Down>  :echoe "Use j"<CR>

" Open new split panes to right and bottom, which feels more natural
set splitbelow
set splitright


" vim-tmux-navigator (ensure these match those in tmux.conf)
let g:tmux_navigator_no_mappings = 1

nnoremap <silent> <C-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <C-j> :TmuxNavigateDown<cr>
nnoremap <silent> <C-k> :TmuxNavigateUp<cr>
nnoremap <silent> <C-l> :TmuxNavigateRight<cr>
nnoremap <silent> <C-\> :TmuxNavigatePrevious<cr>

