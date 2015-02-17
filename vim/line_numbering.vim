set numberwidth=1
set relativenumber
set number

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
nnoremap <silent><leader>n :call NumberToggle()<CR>

