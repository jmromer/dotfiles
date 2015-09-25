" ---------------------- Overrides ------------------------
" <CR>: turn off highlighting by pressing enter
nnoremap <silent><CR> :noh<CR><CR>

" K: Grep for the word under the cursor or visual selection,
"    open results in quickfix pane
nnoremap K yiw:grep! "<C-R>0"<CR>:cw<CR>
vnoremap K y:grep! "<C-R>0"<CR>:cw<CR>

" KW: kill trailing whitespace
" nnoremap <silent><leader>kw :StripWhitespace<CR>
nnoremap KW :StripWhitespace<CR>

" \: (backward slash) to grep-with-ag-to-quickfix shortcut
command! -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
nnoremap \ :Ag ''<LEFT>


" -------------- Leader key mappings (ctrl) -----------------
" C-]: Open ctag in a vertical split
map <leader><C-]> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>


" ------------------- Leader key mappings ---------------------
" -: zoom the current vim pane
nnoremap <leader>- :wincmd _<CR>:wincmd \|<CR>

" =: re-balance vim pane sizes
nnoremap <leader>= :wincmd =<CR>

" as: go to alternative file in a split (Rails.vim, Rake.vim)
nmap <leader>as :AS<CR>

" av: go to alternative file in a vertical split (Rails.vim, Rake.vim)
nmap <leader>av :AV<CR>

" c: copy visual selection to system clipboard
vnoremap <leader>c "*y

" e: open netrw explorer
nnoremap <silent><leader>e :Explore<CR>

" f: Invoke fzf
nnoremap <silent><leader>f :FZF<CR>

" ga: Git add current buffer
nnoremap <silent><leader>ga :Git add %<CR>

" gc: Git commit
nnoremap <silent><leader>gc :Gcommit<CR>

" G: Git
nnoremap <leader>G :Git<SPACE>

" m: (make) run rubocop
nnoremap <leader>m :RuboCop<CR>

" R: From visual mode, leader+R populates command line for search and replace
vnoremap <leader>R y:%s/<C-R>"//g<LEFT><LEFT>

" ra: Reload all open buffers
nnoremap <silent><leader>ra :call ReloadAllBuffers()<CR>

" os: Open Session
nnoremap <leader>os :OpenSession<SPACE>

" sa: visual select all text in file
nnoremap <leader>sa ggVG

" ss: Save Session
nnoremap <leader>ss :SaveSession<SPACE>

" ti: Translation interpolate (in Rails projects)
vnoremap <leader>ti :call I18nTranslateString()<CR>

" td: Translation display (in Rails projects)
vnoremap <leader>td :call I18nDisplayTranslation()<CR>

" w: save buffer
nnoremap <silent><leader>w :w<CR>

" x: cut visual selection to system clipboard
vnoremap <leader>x "*d

" v: paste from system clipboard
nnoremap <leader>v :set paste<CR>i<ESC>"*p:set nopaste<CR>
vnoremap <leader>v d:set paste<CR>i<ESC>"*p:set nopaste<CR>
