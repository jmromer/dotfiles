let g:lightline = {
      \   'colorscheme': 'solarized',
      \   'active': {
      \     'left': [
      \       [ 'mode', 'paste' ],
      \       [ 'fugitive', 'filename' ]
      \     ],
      \     'right': [
      \       [ 'syntastic', 'lineinfo' ],
      \       [ 'percent' ],
      \       [ 'fileformat', 'fileencoding', 'filetype' ]
      \     ]
      \   },
      \   'component_function': {
      \     'mode':         'LLMode',
      \     'fugitive':     'LLFugitive',
      \     'filename':     'LLFilename',
      \     'readonly':     'LLReadonly',
      \     'modified':     'LLModified',
      \     'fileformat':   'LLFileFormat',
      \     'fileencoding': 'LLFileEncoding',
      \     'filetype':     'LLFileType'
      \   },
      \   'component_expand': {
      \     'syntastic': 'SyntasticStatuslineFlag'
      \   },
      \   'component_type': {
      \     'syntastic': 'error'
      \   },
      \   'subseparator': {
      \     'left': '|', 'right': '|'
      \   }
      \ }

function! LLMode()
  let fname = expand('%:t')
  return  fname == '__Gundo__'  ? 'Gundo'  :
        \ fname == '__Gundo_Preview__' ? 'Gundo Preview' :
        \ winwidth(0) < 60 ? '' :
        \ lightline#mode() == 'NORMAL'  ? 'N'  :
        \ lightline#mode() == 'INSERT'  ? 'I'  :
        \ lightline#mode() == 'VISUAL'  ? 'V'  :
        \ lightline#mode() == 'V-LINE'  ? 'VL' :
        \ lightline#mode() == 'V-BLOCK' ? 'VB' :
        \ lightline#mode() == 'REPLACE' ? 'R'  : lightline#mode()
endfunction

function! LLModified()
  return &modified ? '+' : ''
endfunction

function! LLReadonly()
  return &readonly ? '' : ''
endfunction

function! LLFugitive()
  if !exists('*fugitive#head')
    return ''
  endif

  let head = fugitive#head()
  return strlen(head) ? ' ' .head : ''
endfunction

function! LLFilename()
  let fname = expand('%:t')
  return  fname == '__Tagbar__' ? g:lightline.fname :
        \ fname == '__Scratch__' ? 'Scratch' :
        \ fname =~ '__Gundo\|NERD_tree' ? '' :
        \ &ft == 'vimfiler' ? vimfiler#get_status_string() :
        \ &ft == 'unite'    ? unite#get_status_string() :
        \ &ft == 'vimshell' ? vimshell#get_status_string() :
        \ ('' != LLReadonly() ? LLReadonly() . ' ' : '') .
        \ ('' != fname ? fname : '[No Name]') .
        \ ('' != LLModified() ? ' ' . LLModified() : '')
endfunction

function! LLFileEncoding()
  return winwidth(0) > 70 ? (strlen(&fenc) ? &fenc : &enc) : ''
endfunction

function! LLFileFormat()
  return winwidth(0) > 70 ? &fileformat : ''
endfunction

function! LLFileType()
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'file') : ''
endfunction
