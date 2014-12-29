" ----------------------- Tabularize ----------------------------------
if !exists(":Tabularize")
  finish
endif

" Align ruby symbol hashes on the hash marker
AddTabularPattern! rbshash /\s\?\w\+:[^:]/l0l0
AddTabularPattern! rbhash /^[^=]*\zs=>

" Mappings for ruby hash rocket and symbol hashes
nnoremap <silent> <Leader>ahr :Tabularize rbhash<CR>
vnoremap <silent> <Leader>ahr :Tabularize rbhash<CR>
nnoremap <silent> <Leader>ahs :Tabularize rbshash<CR>
vnoremap <silent> <Leader>ahs :Tabularize rbshash<CR>
nnoremap <silent> <Leader>==  :Tabularize /=<CR>
vnoremap <silent> <Leader>==  :Tabularize /=<CR>

" Nice alignment for | based tables

function! s:align()
  let p = '^\s*|\s.*\s|\s*$'
  if exists(':Tabularize') && getline('.') =~# '^\s*|' && (getline(line('.')-1) =~# p || getline(line('.')+1) =~# p)
    let column = strlen(substitute(getline('.')[0:col('.')],'[^|]','','g'))
    let position = strlen(matchstr(getline('.')[0:col('.')],'.*|\s*\zs.*'))
    Tabularize/|/l1
    normal! 0
    call search(repeat('[^|]*|',column).'\s\{-\}'.repeat('.',position),'ce',line('.'))
  endif
endfunction

nnoremap <Leader><Bar> :call s:align()<CR>

