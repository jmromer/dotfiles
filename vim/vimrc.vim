" --------------- Settings by Context ---------------------
runtime color_scheme.vim

runtime plugins.vim
runtime general.vim
runtime plugin_settings.vim

runtime autocommands.vim
runtime windows_and_nav.vim
runtime line_numbering.vim
runtime code_folding.vim

runtime ctrlp.vim
runtime lightline.vim
runtime completion_and_snippets.vim
runtime syntax_checkers.vim
runtime tmux_runners.vim

" ------------------- Leader key mappings ---------------------
" turn off highlighting by pressing enter
nnoremap <silent><CR> :noh<CR><CR>

" reload vimrc
nnoremap <silent><leader>R :source ~/.vimrc<CR>:redraw<CR>:echo "vim reloaded."<CR>

" redraw screen
nnoremap <silent><leader>r :redraw!<CR>

" Fast saving
nnoremap <leader>w :w<CR>

" Fast saving for multiple buffers
nnoremap <leader>wa :wa<CR>

" To save as root, use :W (uses tpope's vim-eunuch)
command! W :SudoWrite

" toggle Tagbar
nnoremap <silent><leader>t :TagbarToggle<CR>

" toggle Buffet's buffer list
nnoremap <silent><leader>b :Bufferlist<CR>

" toggle Gundo
nnoremap <silent><leader>u :GundoToggle<CR>

" kill trailing whitespace
nnoremap <silent><leader>kw :StripWhitespace<CR>

" open netrw explore buffer
nnoremap <silent><C-e> :Explore<CR>

" Open ctag in a vertical split
map <leader><C-]> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>

" Engage Git
nnoremap <leader>gg :Git<space>
nnoremap <silent><leader>ga :Git add %<CR>
nnoremap <silent><leader>gc :Gcommit<CR>

" Open a Scratch buffer
nnoremap <leader>m :Sscratch<CR>

" Show path of current file
map <leader>F :echo expand('%:p')<CR>

" Reload all open buffers
function! ReloadAllBuffers ()
  set autoread
  checktime
endfunction

nnoremap <silent><leader>rr :call ReloadAllBuffers()<CR>


" Session Management
" ------------------
nnoremap <leader>ss :SaveSession<space>
nnoremap <leader>os :OpenSession<space>

" Text search
" ------------
" Grep for the word under the cursor, open results in quickfix pane
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

" Grep with given arguments, open results in quickfix pane
" command! -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
nnoremap <leader>\ :Ag<SPACE>

" ----------------------- Mappings: Copy/Paste ------------------------
" Select all text in file
nnoremap <leader>sa ggVG
" Copy to system clipboard
vnoremap <leader>c "*y
" Cut to system clipboard
vnoremap <leader>x "*d
" Paste from system clipboard
nnoremap <silent><leader>v :set paste<CR>i<ESC>"*p:set nopaste<CR>
vnoremap <silent><leader>v d:set paste<CR>i<ESC>"*p:set nopaste<CR>

" -------------- Search and Replace cursor selection  ----------------
function! CmdLine(str)
    exe "menu Foo.Bar :" . a:str
    emenu Foo.Bar
    unmenu Foo
endfunction

function! SearchAndReplace() range
    let l:saved_reg = @"
    execute "normal! vgvy"
    let l:pattern = escape(@", '\\/.*$^~[]')
    let l:pattern = substitute(l:pattern, "\n$", "", "")
    call CmdLine("%s" . '/'. l:pattern . '//g')
    let @/ = l:pattern
    let @" = l:saved_reg
endfunction

" from visual mode, leader+r populates command line
vnoremap <silent> <leader>r :call SearchAndReplace()<CR>

