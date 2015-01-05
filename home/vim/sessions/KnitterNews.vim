" ~/.dotfiles/home/vim/sessions/KnitterNews.vim:
" Vim session script.
" Created by session.vim 2.7 on 06 January 2015 at 08:14:46.
" Open this file in Vim and run :source % to restore your session.

if exists('g:syntax_on') != 1 | syntax on | endif
if exists('g:did_load_filetypes') != 1 | filetype on | endif
if exists('g:did_load_ftplugin') != 1 | filetype plugin on | endif
if exists('g:did_indent_on') != 1 | filetype indent on | endif
if &background != 'dark'
	set background=dark
endif
if !exists('g:colors_name') || g:colors_name != 'Twilight' | colorscheme Twilight | endif
call setqflist([{'lnum': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/controllers/votes_controller.rb', 'text': 'create'}, {'lnum': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'get ''votes/destroy''', 'text': 'route'}, {'lnum': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'get ''votes/create''', 'text': 'route'}, {'lnum': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/views/votes', 'text': 'create  '}, {'lnum': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/views/votes/create.html.erb', 'text': 'create  '}, {'lnum': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/views/votes/destroy.html.erb', 'text': 'create  '}, {'lnum': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/helpers/votes_helper.rb', 'text': 'create  '}, {'lnum': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/assets/javascripts/votes.coffee', 'text': 'create    '}, {'lnum': 0, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/assets/stylesheets/votes.scss', 'text': 'create    '}])
let SessionLoad = 1
if &cp | set nocp | endif
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/Developer/KnitterNews
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +3 app/views/links/_link_details.html.erb
badd +1 app/views/links/_link.html.erb
badd +1 config/routes.rb
badd +22 app/controllers/votes_controller.rb
badd +1 app/views/votes
badd +1 app/views/votes/create.html.erb
badd +1 app/views/votes/destroy.html.erb
badd +1 app/views/votes/_vote_form.html.erb
badd +6 app/models/user.rb
badd +0 app/views/pages/credentials.html.erb
badd +3 app/controllers/users_controller.rb
badd +1 app/controllers/links_controller.rb
badd +5 app/models/link.rb
badd +9 app/views/votes/create.js.erb
badd +0 app/views/votes/_unlike_form.html.erb
argglobal
silent! argdel *
argadd ~/Developer/KnitterNews
edit app/controllers/votes_controller.rb
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
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
wincmd w
wincmd _ | wincmd |
split
1wincmd k
wincmd w
wincmd t
set winheight=1 winwidth=1
exe '1resize ' . ((&lines * 5 + 16) / 33)
exe 'vert 1resize ' . ((&columns * 84 + 84) / 169)
exe '2resize ' . ((&lines * 5 + 16) / 33)
exe 'vert 2resize ' . ((&columns * 84 + 84) / 169)
exe '3resize ' . ((&lines * 5 + 16) / 33)
exe 'vert 3resize ' . ((&columns * 84 + 84) / 169)
exe '4resize ' . ((&lines * 11 + 16) / 33)
exe 'vert 4resize ' . ((&columns * 84 + 84) / 169)
exe '5resize ' . ((&lines * 23 + 16) / 33)
exe 'vert 5resize ' . ((&columns * 84 + 84) / 169)
exe '6resize ' . ((&lines * 5 + 16) / 33)
exe 'vert 6resize ' . ((&columns * 84 + 84) / 169)
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=2
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
6,8fold
5,9fold
12,14fold
11,15fold
19,24fold
1,25fold
1
silent! normal! zo
5
silent! normal! zo
6
silent! normal! zo
11
silent! normal! zo
12
silent! normal! zo
let s:l = 11 - ((2 * winheight(0) + 2) / 5)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
11
normal! 0
lcd ~/Developer/KnitterNews
wincmd w
argglobal
edit ~/Developer/KnitterNews/app/views/votes/_vote_form.html.erb
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=1
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 1 - ((0 * winheight(0) + 2) / 5)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 0
lcd ~/Developer/KnitterNews
wincmd w
argglobal
edit ~/Developer/KnitterNews/app/views/links/_link_details.html.erb
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=3
setlocal fml=1
setlocal fdn=20
setlocal fen
6
silent! normal! zo
15
silent! normal! zo
let s:l = 6 - ((2 * winheight(0) + 2) / 5)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
6
normal! 026|
lcd ~/Developer/KnitterNews
wincmd w
argglobal
edit ~/Developer/KnitterNews/app/views/links/_link.html.erb
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=1
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 1 - ((0 * winheight(0) + 5) / 11)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 033|
lcd ~/Developer/KnitterNews
wincmd w
argglobal
edit ~/Developer/KnitterNews/app/views/votes/create.js.erb
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=1
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 7 - ((6 * winheight(0) + 11) / 23)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
7
normal! 05|
lcd ~/Developer/KnitterNews
wincmd w
argglobal
edit ~/Developer/KnitterNews/app/views/votes/_unlike_form.html.erb
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=1
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 1 - ((0 * winheight(0) + 2) / 5)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 05|
lcd ~/Developer/KnitterNews
wincmd w
5wincmd w
exe '1resize ' . ((&lines * 5 + 16) / 33)
exe 'vert 1resize ' . ((&columns * 84 + 84) / 169)
exe '2resize ' . ((&lines * 5 + 16) / 33)
exe 'vert 2resize ' . ((&columns * 84 + 84) / 169)
exe '3resize ' . ((&lines * 5 + 16) / 33)
exe 'vert 3resize ' . ((&columns * 84 + 84) / 169)
exe '4resize ' . ((&lines * 11 + 16) / 33)
exe 'vert 4resize ' . ((&columns * 84 + 84) / 169)
exe '5resize ' . ((&lines * 23 + 16) / 33)
exe 'vert 5resize ' . ((&columns * 84 + 84) / 169)
exe '6resize ' . ((&lines * 5 + 16) / 33)
exe 'vert 6resize ' . ((&columns * 84 + 84) / 169)
tabnext 1
if exists('s:wipebuf')
"   silent exe 'bwipe ' . s:wipebuf
endif
" unlet! s:wipebuf
set winheight=999 winwidth=84 shortmess=a
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save

" Support for special windows like quick-fix and plug-in windows.
" Everything down here is generated by vim-session (not supported
" by :mksession out of the box).

5wincmd w
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
