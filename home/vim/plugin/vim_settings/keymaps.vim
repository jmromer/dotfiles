" ---------------------- Overrides ------------------------

" CR: turn off highlighting by pressing enter
nnoremap <silent><CR> :noh<CR><CR>

" K: Grep for the word under the cursor, open results in quickfix pane
nnoremap K :Ag! "\b<C-R><C-W>\b"<CR>:cw<CR>


" -------------- Leader key mappings (ctrl) -----------------

" C-r: reload vimrc
nnoremap <silent><leader><C-r> :source ~/.vimrc<CR>:redraw<CR>:echo 'reloaded'<CR>

" C-]: Open ctag in a vertical split
map <leader><C-]> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>

" C-\: Switch between the previous two files
nnoremap <C-\> <c-^>

" <C-w> t: Open a new tab
nnoremap <silent> <C-w>t :tabnew<CR>


" ------------- Leader key mappings (shift) -----------------

" S-r: Reload all open buffers
nnoremap <silent><leader><S-r> :call ReloadAllBuffers()<CR>


" ------------------- Leader key mappings ---------------------

" -: zoom the current pane
nnoremap <leader>- :wincmd _<CR>:wincmd \|<CR>

" =: re-balance pane sizes
nnoremap <leader>= :wincmd =<CR>

" bp: insert a binding.pry
nmap <leader>bp orequire 'pry'; binding.pry<ESC>

" c: copy visual selection to system clipboard
vnoremap <leader>c "*y

" e: open netrw explore buffer
nnoremap <silent><leader>e :Explore<CR>

" f: Invoke fzf
nnoremap <silent><leader>f :FZF<CR>

" ga: Git add current buffer
nnoremap <silent><leader>ga :Git add %<CR>

" gc: Git commit
nnoremap <silent><leader>gc :Gcommit<CR>

" gg: Git
nnoremap <leader>gg :Git<space>

" kw: kill trailing whitespace
nnoremap <silent><leader>kw :StripWhitespace<CR>

" n: toggle relative numbering
nnoremap <silent><leader>n :call NumberToggle()<CR>

" r: From visual mode, leader+r populates command line for search and replace
vnoremap <silent> <leader>r :call SearchAndReplace()<CR>

" R: redraw the screen
nnoremap <silent><leader>R :redraw!<CR>

" sa: visual select all text in file
nnoremap <leader>sa ggVG

" ss: Save Session
nnoremap <leader>ss :SaveSession<space>

" os: Open Session
nnoremap <leader>os :OpenSession<space>

" v: paste from system clipboard
nnoremap <silent><leader>v :set paste<CR>i<ESC>"*p:set nopaste<CR>
vnoremap <silent><leader>v d:set paste<CR>i<ESC>"*p:set nopaste<CR>

" w: Fast saving for all buffers
nnoremap <silent><leader>w :wa<CR>

" x: cut visual selection to system clipboard
vnoremap <leader>x "*d
