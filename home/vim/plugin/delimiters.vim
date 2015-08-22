function! IsEmptyPair(str)
  for pair in split(&matchpairs, ',') + [ "''", '""', '``' ]
    if a:str == join(split(pair, ':'),'')
      return 1
    endif
  endfor

  return 0
endfunc

function! SkipDelim(char)
  let cur = strpart(getline('.'), col('.')-2, 3)

  if cur[0] == "\\"
    return a:char
  elseif cur[1] == a:char
    return "\<Right>"
  elseif cur[1] == ' ' && cur[2] == a:char
    return "\<Right>\<Right>"
  elseif IsEmptyPair(cur[0] . a:char)
    return a:char . "\<Left>"
  else
    return a:char
  endif
endfunc

inoremap <expr> ) SkipDelim(')')
inoremap <expr> ] SkipDelim(']')
inoremap <expr> } SkipDelim('}')
inoremap <expr> ' SkipDelim("'")
inoremap <expr> " SkipDelim('"')
inoremap <expr> ` SkipDelim('`')
