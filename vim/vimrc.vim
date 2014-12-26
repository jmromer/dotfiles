" ----------------- Base Settings ------------------------
set nocompatible        " use vim, not vi, settings.
set t_Co=256            " 256 color terminal
set background=dark
colorscheme Twilight

" --------------- Settings by Context ---------------------
runtime bundles.vim
runtime general.vim
runtime plugins.vim

runtime autocommands.vim
runtime windows_and_nav.vim
runtime line_numbering.vim
runtime code_folding.vim
runtime syntax.vim

runtime ctrlp.vim
runtime tabularize.vim
runtime airline.vim
runtime git.vim
runtime test_runners.vim

" ------------------- Leader key mappings ---------------------
" run commands in an interactive shell
nnoremap <Leader>e :RunInInteractiveShell<space>
" turn off highlighting by pressing enter
nnoremap <silent><CR> :noh<CR><CR>
" reload vimrc
nnoremap <silent><Leader>r :source ~/.vimrc<CR>:echoe "vim reloaded."<CR>
" redraw screen
nnoremap <silent><Leader>rr :redraw!<CR>
" toggle Tagbar
nnoremap <silent><S-t> :TagbarToggle<CR>
" toggle Buffet's buffer list
nnoremap <silent><leader>b :Bufferlist<CR>
" toggle Gundo
nnoremap <silent><leader>u :GundoToggle<CR>
" kill trailing whitespace
nnoremap <silent><Leader>kw :EraseBadWhitespace<CR>
" open netrw explore buffer
nnoremap <silent><C-e> :Explore<CR>
" see YankRing contents
nnoremap <silent><Leader>v :YRShow<CR>
" Open ctag in a vertical split
map <leader><C-]> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>

" Session Management
" ------------------
nnoremap <Leader>ss :SaveSession<space>
nnoremap <Leader>os :OpenSession<space>

" Text search
" ------------
" Grep for the word under the cursor, open results in quickfix pane
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

" Grep with given arguments, open results in quickfix pane
command! -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
nnoremap <Leader>\ :Ag<SPACE>

" ----------------------- Mappings: Copy/Paste ------------------------
" Select all text in file
nnoremap <Leader>a ggVG
" Copy to system clipboard
vnoremap <Leader>c "*y
" Cut to system clipboard
vnoremap <Leader>x "*d
" Paste from system clipboard
nnoremap <silent><Leader>p :set paste<CR>i<ESC>"*p:set nopaste<CR>
vnoremap <silent><Leader>p D:set paste<CR>i<ESC>"*p:set nopaste<CR>

