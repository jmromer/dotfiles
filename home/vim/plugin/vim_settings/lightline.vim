let g:lightline = {
      \   'colorscheme': 'solarized',
      \   'active': {
      \     'left': [
      \       [ 'mode', 'paste' ],
      \       [ 'fugitive', 'filename' ],
      \       [ 'ctrlpmark']
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
      \     'ctrlpmark':    'CtrlPMark',
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

" let g:syntastic_mode_map = {
"       \   'mode': 'passive',
"       \   'active_filetypes': ['rb']
"       \ }

function! LLMode()
  let fname = expand('%:t')
  return  fname == '__Tagbar__' ? 'Tagbar' :
        \ fname == 'ControlP'   ? 'CtrlP'  :
        \ fname == '__Gundo__'  ? 'Gundo'  :
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
  if &filetype == 'help'
    return ''
  elseif &modified
    return '+'
  elseif &modifiable
    return ''
  else
    return ''
  endif
endfunction

function! LLReadonly()
  if &filetype == 'help'
    return ''
  elseif &readonly
    return ''
  else
    return ''
  endif
endfunction

function! LLFugitive()
  if exists('*fugitive#head')
    let _ = fugitive#head()
    return strlen(_) ? ' ' ._ : ''
  endif
  return ''
endfunction

function! LLFilename()
  let fname = expand('%:t')
  return  fname == 'ControlP'   ? g:lightline.ctrlp_item :
        \ fname == '__Tagbar__' ? g:lightline.fname :
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
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
endfunction

augroup AutoSyntastic
    autocmd!
    autocmd BufWritePost * call s:syntastic()
augroup END

function! s:syntastic()
  SyntasticCheck
  call lightline#update()
endfunction

