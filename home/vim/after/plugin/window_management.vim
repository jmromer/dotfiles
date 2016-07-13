" -------------- Navigation / Window Management ------------------------ "
" Open new split panes to right and bottom
set splitbelow
set splitright

" --- window resizing ---
nnoremap <Left>  <C-w><
nnoremap <Right> <C-w>>
nnoremap <Down>  <C-w>+
nnoremap <Up>    <C-w>-

" --- buffer navigation ---
" C-e: Scroll window up by 5 lines
nnoremap <C-e> 5<C-e>

" C-y: Scroll window down by 5 lines
nnoremap <C-y> 5<C-y>

" --- tab navigation ---
" S-h: previous tab
nnoremap <S-h> gT

" S-l: next tab
nnoremap <S-l> gt
