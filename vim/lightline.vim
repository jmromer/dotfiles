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

function! CtrlPMark()
  if expand('%:t') =~ 'ControlP'
    call lightline#link('iR'[g:lightline.ctrlp_regex])
    return lightline#concatenate([g:lightline.ctrlp_prev, g:lightline.ctrlp_item
          \ , g:lightline.ctrlp_next], 0)
  else
    return ''
  endif
endfunction

let g:ctrlp_status_func = {
  \ 'main': 'CtrlPStatusFunc_1',
  \ 'prog': 'CtrlPStatusFunc_2',
  \ }

function! CtrlPStatusFunc_1(focus, byfname, regex, prev, item, next, marked)
  let g:lightline.ctrlp_regex = a:regex
  let g:lightline.ctrlp_prev = a:prev
  let g:lightline.ctrlp_item = a:item
  let g:lightline.ctrlp_next = a:next
  return lightline#statusline(0)
endfunction

function! CtrlPStatusFunc_2(str)
  return lightline#statusline(0)
endfunction

let g:tagbar_status_func = 'TagbarStatusFunc'

function! TagbarStatusFunc(current, sort, fname, ...) abort
    let g:lightline.fname = a:fname
  return lightline#statusline(0)
endfunction

" modes
" \ fname =~ 'NERD_tree' ? 'NERDTree' :
" \ &ft == 'unite' ? 'Unite' :
" \ &ft == 'vimfiler' ? 'VimFiler' :
" \ &ft == 'vimshell' ? 'VimShell' :

" plugin settings
" let g:unite_force_overwrite_statusline = 0
" let g:vimfiler_force_overwrite_statusline = 0
" let g:vimshell_force_overwrite_statusline = 0
"

"   \ 'separator': { 'left': '', 'right': '' },
"   \ 'subseparator': { 'left': '', 'right': '' }

