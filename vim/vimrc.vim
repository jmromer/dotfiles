" ----------------- Base Settings ------------------------
set nocompatible        " use vim, not vi, settings.
set t_Co=256            " 256 color terminal
set background=dark
colorscheme Twilight
filetype plugin on
filetype indent on

" --------------- Settings by Context ---------------------
runtime bundles.vim
runtime general.vim
runtime plugins.vim

runtime autocommands.vim
runtime windows_and_nav.vim
runtime line_numbering.vim
runtime code_folding.vim
runtime completion_and_snippets.vim
runtime syntax.vim

runtime ctrlp.vim
runtime tabularize.vim
runtime airline.vim
runtime git.vim
runtime tmux_runners.vim

" ------------------- Leader key mappings ---------------------
" turn off highlighting by pressing enter
nnoremap <silent><CR> :noh<CR><CR>
" reload vimrc
nnoremap <silent><Leader>r :source ~/.vimrc<CR>:redraw<CR>:echo "vim reloaded."<CR>
" redraw screen
nnoremap <silent><Leader>R :redraw!<CR>
" Fast saving
nnoremap <Leader>w :w<CR>
" Fast saving for multiple buffers
nnoremap <Leader>wa :wa<CR>
" For saving read-only files: to save as root, use :W
command! W w !sudo tee % > /dev/null
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
nnoremap <Leader>sa ggVG
" Copy to system clipboard
vnoremap <Leader>c "*y
" Cut to system clipboard
vnoremap <Leader>x "*d
" Paste from system clipboard
nnoremap <silent><Leader>v :set paste<CR>i<ESC>"*p:set nopaste<CR>
vnoremap <silent><Leader>v d:set paste<CR>i<ESC>"*p:set nopaste<CR>

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

