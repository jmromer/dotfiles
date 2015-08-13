vmap <unique> <C-k> <Plug>SchleppUp
vmap <unique> <C-j> <Plug>SchleppDown
vmap <unique> <C-h> <Plug>SchleppLeft
vmap <unique> <C-l> <Plug>SchleppRight

" Allow moving left even if text is there (Default is false)
let g:Schlepp#allowSquishingLines = 0
let g:Schlepp#allowSquishingBlocks = 0

" Remove trailing whitespace on block move
let g:Schlepp#trimWS = 0

" Schlepp can also reindent code as it moves. Any of these work
"
" SchleppUp with reindentation is SchleppIndentUp
" SchleppDown with reindentation is SchleppIndentDown
" let g:Schlepp#reindent = 1
" map to the toggle function (below) - This is how I like to use it
" vmap <unique> i <Plug>SchleppToggleReindent
