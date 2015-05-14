" -------------- Search and Replace cursor selection  ----------------
function! CmdLine(str)
    exe "menu Foo.Bar :" . a:str
    emenu Foo.Bar
    unmenu Foo
endfunction

function! SearchAndReplace() range
    let l:saved_reg = @"
    execute "normal! vgvy"
    let l:pattern = escape(@", '\\/.*$^~[]')
    let l:pattern = substitute(l:pattern, "\n$", "", "")
    call CmdLine("%s" . '/'. l:pattern . '//g')
    let @/ = l:pattern
    let @" = l:saved_reg
endfunction
