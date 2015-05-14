" EasyAlign: Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
vmap <Enter> <Plug>(EasyAlign)

" EasyAlign: Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Add " delimiter for VimL
let g:easy_align_delimiters = {}
let g:easy_align_delimiters['"'] = {
\ 'pattern': '\(\s\+"\)',
\ 'left_margin': 0, 'right_margin': 1
\ }
