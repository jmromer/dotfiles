" ~/.dotfiles/home/vim/sessions/admin_panel.vim:
" Vim session script.
" Created by session.vim 2.9 on 20 February 2015 at 14:04:05.
" Open this file in Vim and run :source % to restore your session.

if exists('g:syntax_on') != 1 | syntax on | endif
if exists('g:did_load_filetypes') != 1 | filetype on | endif
if exists('g:did_load_ftplugin') != 1 | filetype plugin on | endif
if exists('g:did_indent_on') != 1 | filetype indent on | endif
if &background != 'dark'
	set background=dark
endif
if !exists('g:colors_name') || g:colors_name != 'solarized' | colorscheme solarized | endif
call setqflist([])
let SessionLoad = 1
if &cp | set nocp | endif
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/Travel/Farespotter
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +22 app/controllers/lowfares/deal_images_controller.rb
badd +12 app/models/deal_image.rb
badd +18 config/application.rb
badd +6 app/models/concerns/filterable.rb
badd +5 config/environment.rb
badd +6 config/initializers/preload_models.rb
badd +68 app/controllers/application_controller.rb
badd +12 app/views/layouts/lowfares/admin.html.erb
badd +0 app/assets/javascripts/deal_images/routers/image-router.coffee
badd +0 app/assets/javascripts/deal_images/views/images-collection.coffee
argglobal
silent! argdel *
argadd ~/Travel/Farespotter
edit app/controllers/lowfares/deal_images_controller.rb
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd _ | wincmd |
split
wincmd _ | wincmd |
split
wincmd _ | wincmd |
split
3wincmd k
wincmd w
wincmd w
wincmd w
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 58 + 71) / 143)
exe '2resize ' . ((&lines * 5 + 21) / 43)
exe 'vert 2resize ' . ((&columns * 84 + 71) / 143)
exe '3resize ' . ((&lines * 22 + 21) / 43)
exe 'vert 3resize ' . ((&columns * 84 + 71) / 143)
exe '4resize ' . ((&lines * 5 + 21) / 43)
exe 'vert 4resize ' . ((&columns * 84 + 71) / 143)
exe '5resize ' . ((&lines * 5 + 21) / 43)
exe 'vert 5resize ' . ((&columns * 84 + 71) / 143)
argglobal
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=4
setlocal fml=1
setlocal fdn=20
setlocal fen
7
silent! normal! zo
23
silent! normal! zo
let s:l = 56 - ((28 * winheight(0) + 20) / 40)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
56
normal! 043|
lcd ~/Travel/Farespotter
wincmd w
argglobal
edit app/models/concerns/filterable.rb
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=1
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
5,8fold
4,9fold
1,10fold
1
silent! normal! zo
4
silent! normal! zo
5
silent! normal! zo
let s:l = 8 - ((2 * winheight(0) + 2) / 5)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
8
normal! 05|
lcd ~/Travel/Farespotter
wincmd w
argglobal
edit ~/Travel/Farespotter/app/models/deal_image.rb
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=1
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
16,19fold
21,22fold
24,29fold
23,30fold
32,34fold
36,41fold
44,48fold
43,49fold
1,50fold
1
silent! normal! zo
let s:l = 15 - ((10 * winheight(0) + 11) / 22)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
15
normal! 03|
lcd ~/Travel/Farespotter
wincmd w
argglobal
edit ~/Travel/Farespotter/app/assets/javascripts/deal_images/routers/image-router.coffee
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=4
setlocal fml=1
setlocal fdn=20
setlocal fen
2
silent! normal! zo
let s:l = 14 - ((2 * winheight(0) + 2) / 5)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
14
normal! 014|
lcd ~/Travel/Farespotter
wincmd w
argglobal
edit ~/Travel/Farespotter/app/assets/javascripts/deal_images/views/images-collection.coffee
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=4
setlocal fml=1
setlocal fdn=20
setlocal fen
2
silent! normal! zo
7
silent! normal! zo
let s:l = 4 - ((1 * winheight(0) + 2) / 5)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
4
normal! 016|
lcd ~/Travel/Farespotter
wincmd w
3wincmd w
exe 'vert 1resize ' . ((&columns * 58 + 71) / 143)
exe '2resize ' . ((&lines * 5 + 21) / 43)
exe 'vert 2resize ' . ((&columns * 84 + 71) / 143)
exe '3resize ' . ((&lines * 22 + 21) / 43)
exe 'vert 3resize ' . ((&columns * 84 + 71) / 143)
exe '4resize ' . ((&lines * 5 + 21) / 43)
exe 'vert 4resize ' . ((&columns * 84 + 71) / 143)
exe '5resize ' . ((&lines * 5 + 21) / 43)
exe 'vert 5resize ' . ((&columns * 84 + 71) / 143)
tabnext 1
if exists('s:wipebuf')
"   silent exe 'bwipe ' . s:wipebuf
endif
" unlet! s:wipebuf
set winheight=999 winwidth=84 shortmess=ac
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save

" Support for special windows like quick-fix and plug-in windows.
" Everything down here is generated by vim-session (not supported
" by :mksession out of the box).

3wincmd w
tabnext 1
if exists('s:wipebuf')
  if empty(bufname(s:wipebuf))
if !getbufvar(s:wipebuf, '&modified')
  let s:wipebuflines = getbufline(s:wipebuf, 1, '$')
  if len(s:wipebuflines) <= 1 && empty(get(s:wipebuflines, 0, ''))
    silent execute 'bwipeout' s:wipebuf
  endif
endif
  endif
endif
doautoall SessionLoadPost
unlet SessionLoad
" vim: ft=vim ro nowrap smc=128
