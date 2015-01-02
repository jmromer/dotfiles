set relativenumber
set numberwidth=4

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

" Toggle numbering style with leader+n
nnoremap <silent><Leader>n :call NumberToggle()<CR>

