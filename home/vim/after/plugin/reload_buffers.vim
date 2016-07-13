" Reload all open buffers
function! ReloadAllBuffers ()
  set autoread
  checktime
  echo "Buffers reloaded"
endfunction

command! RA :call ReloadAllBuffers()
