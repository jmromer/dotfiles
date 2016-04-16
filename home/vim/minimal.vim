set nocompatible
filetype plugin on

setlocal noswapfile       " don't keep a swapfile
setlocal bufhidden=unload " unload buffers once they're not visible
setlocal undolevels=1     " only one undo allowed

" disable event bindings used by augroups for syntax highlighting, etc.
set eventignore+=FileType
set eventignore+=VimEnter
set eventignore+=Syntax
