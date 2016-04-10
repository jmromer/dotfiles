" --- coordinate navigation with tmux ---
" vim-tmux-navigator (ensure these match those in tmux.conf)
let g:tmux_navigator_no_mappings = 1

nnoremap <silent> <C-\> :TmuxNavigatePrevious<CR>
nnoremap <silent> <C-k> :TmuxNavigateUp<CR>
nnoremap <silent> <C-j> :TmuxNavigateDown<CR>
nnoremap <silent> <C-l> :TmuxNavigateRight<CR>
" nnoremap <silent> <C-h> :TmuxNavigateLeft<CR>

if has('nvim')
  nnoremap <silent> <BS> :<C-u>TmuxNavigateLeft<CR>
else
  nnoremap <silent> <C-h> :TmuxNavigateLeft<CR>
endif

