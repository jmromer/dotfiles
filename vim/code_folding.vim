set foldmethod=syntax " Enable folding based on syntax rules
set foldlevelstart=1  " Initial fold only 1 level deep

" Custom folding function
if has("folding")
  set foldtext=MyFoldText()

  function! MyFoldText()
    if v:version < 701
      return foldtext()
    endif

    " clear fold from fillchars to set it up the way we want later
    let &l:fillchars = substitute(&l:fillchars,',\?fold:.','','gi')
    let l:numwidth = (v:version < 701 ? 8 : &numberwidth)

    if b:foldpat==1
      let l:align = winwidth(0)-&foldcolumn-(&nu ? Max(strlen(line('$'))+1, l:numwidth) : 0)
      let l:foldtext = ' '.v:folddashes
      let l:linetext = substitute(getline(v:foldstart),'\s\+$','','')
      let l:linetext .= ' ---'.(v:foldend-v:foldstart-1).' lines--- '
      let l:linetext .= substitute(getline(v:foldend),'^\s\+','','')
      let l:linetext = strpart(l:linetext,0,l:align-strlen(l:foldtext))
      let l:align -= strlen(l:linetext)
      setlocal fillchars+=fold:-
      return printf('%s%*s', l:linetext, l:align, l:foldtext)
    endif

    if &fdm=='diff'
      let l:foldtext = ' '.(v:foldend-v:foldstart).' lines the same -------'.v:folddashes.'|'
    elseif !exists('b:foldpat') || b:foldpat==0
      let l:foldtext = ' '.(v:foldend-v:foldstart).' lines '.v:folddashes.'|'
    endif

    let l:endofline = (&textwidth>0 ? &textwidth : 80)
    let l:linetext = strpart(getline(v:foldstart),0,l:endofline-strlen(l:foldtext))
    let l:align = l:endofline-strlen(l:linetext)
    return printf('%s%*s', l:linetext, l:align, l:foldtext)
  endfunction
endif
