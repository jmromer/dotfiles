set number
set numberwidth=4

highlight LineNr term=bold cterm=NONE ctermfg=DarkGrey ctermbg=NONE gui=NONE guifg=DarkGrey guibg=NONE

" Toggle relative and absolute numbering
function! NumberToggle()
  if &relativenumber == 1
    set norelativenumber
    set number
  else
    set relativenumber
    set number
  endif
endfunction

nnoremap <silent><C-n> :call NumberToggle()<CR>

