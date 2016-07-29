" ---------------------- Overrides ------------------------
" <CR>: turn off highlighting by pressing enter
nnoremap <silent><CR> :noh<CR><CR>

" \: (backward slash) to grep-with-ag-to-quickfix shortcut
command! -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
nnoremap <leader>/ :Ag ''<LEFT>

" K: Grep for the word under the cursor or visual selection,
"    open results in quickfix pane
nnoremap <silent><S-k> yiw:grep! "<C-R>0"<CR>:cw<CR>
vnoremap <silent><S-k> y:grep! "<C-R>0"<CR>:cw<CR>

" C-Space to exit insert mode
inoremap <C-Space> <ESC>

" -------------- Leader key mappings (ctrl) -----------------
" C-]: Open ctag in a vertical split
map <silent><leader><C-]> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>

" ]: Open ctag in a horizontal split
map <silent><leader>] :sp <CR>:exec("tag ".expand("<cword>"))<CR>

" ------------------- Leader key mappings ---------------------
" -: zoom the current vim pane
nnoremap <leader>- :wincmd _<CR>:wincmd \|<CR>

" =: re-balance vim pane sizes
nnoremap <leader>= :wincmd =<CR>

" ff: Invoke fzf
nnoremap <silent><leader>ff :FZF<CR>

" fs: save buffer
nnoremap <silent><leader>fs :StripWhitespace<CR>:w<CR>

" g: Git
nnoremap <leader>g :Git<SPACE>

if has('nvim')
  " m: (make) run Neomake. Use [l and ]l to cycle through loc list
  nnoremap <leader>m :Neomake<CR>
endif

" R: From visual mode, leader+R populates command line for search and replace
vnoremap <leader>R y:%s/<C-R>"//g<LEFT><LEFT>

" R: From normal mode, redraws
nnoremap <leader>R :redraw!<CR>

" os: Open Session
nnoremap <leader>os :OpenSession<SPACE>

" wv: open vertical split
nnoremap <leader>wv <C-w>v

" ws: open split
nnoremap <leader>ws <C-w>s

" wh: navigate pane left
nnoremap <leader>wh <C-w>h

" wj: navigate pane down
nnoremap <leader>wj <C-w>j

" wk: navigate pane up
nnoremap <leader>wk <C-w>k

" wl: navigate pane right
nnoremap <leader>wl <C-w>l

" wq: close window
nnoremap <leader>wq :quit<CR>

" ss: Save Session
nnoremap <leader>ss :SaveSession<SPACE>

" x: cut visual selection to system clipboard
vnoremap <leader>x "*d

" v: paste from system clipboard
nnoremap <leader>v :set paste<CR>i<ESC>"*p:set nopaste<CR>
vnoremap <leader>v d:set paste<CR>i<ESC>"*p:set nopaste<CR>
