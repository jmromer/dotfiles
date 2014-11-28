" ~/Dropbox/Sync/Preferences/vim/sessions/dot.vim:
" Vim session script.
" Created by session.vim 2.7 on 27 November 2014 at 02:07:31.
" Open this file in Vim and run :source % to restore your session.

if exists('g:syntax_on') != 1 | syntax on | endif
if exists('g:did_load_filetypes') != 1 | filetype on | endif
if exists('g:did_load_ftplugin') != 1 | filetype plugin on | endif
if exists('g:did_indent_on') != 1 | filetype indent on | endif
if &background != 'dark'
	set background=dark
endif
if !exists('g:colors_name') || g:colors_name != 'twilight256' | colorscheme twilight256 | endif
call setqflist([{'lnum': 28, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'git/gitignore', 'text': '# Ignore the default SQLite database.'}, {'lnum': 11, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'home/laptop.local', 'text': '  mongodb                       # document-based NoSQL database'}, {'lnum': 38, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'home/laptop.local', 'text': '  "gnu-which --default-names"     # which: Find location of given executable'}, {'lnum': 56, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'home/pryrc', 'text': '  # Use Hirb for tabular output'}, {'lnum': 38, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'home/vimrc', 'text': '" Tab completion'}, {'lnum': 39, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'home/vimrc', 'text': '" will insert tab at beginning of line,'}, {'lnum': 42, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'home/vimrc', 'text': 'function! InsertTabWrapper()'}, {'lnum': 45, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'home/vimrc', 'text': '        return "\<tab>"'}, {'lnum': 50, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'home/vimrc', 'text': 'inoremap <Tab> <c-r>=InsertTabWrapper()<cr>'}, {'lnum': 51, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'home/vimrc', 'text': 'inoremap <S-Tab> <c-n>'}, {'lnum': 25, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'home/zsh/completion/_bundler', 'text': '      "gem[Create a simple gem, suitable for development with bundler]" \'}, {'lnum': 15, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'home/zshrc', 'text': '# load custom executable functions'}, {'lnum': 24, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'osx/customize_applications.sh', 'text': 'fecho "Linking to Stickies Database"'}, {'lnum': 25, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'osx/customize_applications.sh', 'text': 'rm -f $HOME/Library/StickiesDatabase'}, {'lnum': 26, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'osx/customize_applications.sh', 'text': 'ln -s $SYNCED_PREFERENCES/StickiesDatabase $HOME/Library/StickiesDatabase'}, {'lnum': 28, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'osx/customize_applications.sh', 'text': 'fecho "Linking to Stickies Database"'}, {'lnum': 8, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'osx/set_defaults.sh', 'text': 'defaults write NSGlobalDomain NSTableViewDefaultSizeMode -int 2'}, {'lnum': 15, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'osx/set_defaults.sh', 'text': 'defaults write org.n8gray.QLColorCode extraHLFlags ''--wrap --replace-tabs=2'''}, {'lnum': 102, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'shell/aliases.sh', 'text': 'alias hreset=''heroku pg:reset DATABASE --confirm'''}, {'lnum': 75, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'shell/git.sh', 'text': '    local branch=${BASH_REMATCH[1]:-$match[1]} # bash/zsh portable'}, {'lnum': 14, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'vim/bundles.vim', 'text': 'Plugin ''ervandew/supertab''             " context-aware tab-completion'}, {'lnum': 25, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'vim/bundles.vim', 'text': 'Plugin ''godlygeek/tabular''       " align on a given regex (e.g., =)'}, {'lnum': 2, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'vim/ctrlp.vim', 'text': 'if executable(''ag'')'}, {'lnum': 34, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'vim/ctrlp.vim', 'text': '" jump when <cr> is pressed, but only to windows in the current tab.'}, {'lnum': 73, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'vim/vimrc.local', 'text': '"" shift-h / shift-l to switch tabs'}, {'lnum': 100, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'vim/vimrc.local', 'text': '" Indentation with soft tabs, 2 spaces'}, {'lnum': 101, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'vim/vimrc.local', 'text': 'set expandtab      " use soft tabs'}, {'lnum': 102, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'vim/vimrc.local', 'text': 'set shiftwidth=2   " spaces per tab (when shifting)'}, {'lnum': 103, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'vim/vimrc.local', 'text': 'set softtabstop=2  " 2-space soft tabs'}, {'lnum': 109, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'vim/vimrc.local', 'text': 'set list listchars=tab:»·,trail:·,nbsp:·'}, {'lnum': 156, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'vim/vimrc.local', 'text': 'vnoremap <Leader>== :Tabularize /=<CR>'}, {'lnum': 160, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'vim/vimrc.local', 'text': '  Tabularize /\w:\zs/l0l1'}, {'lnum': 164, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'vim/vimrc.local', 'text': '  Tabularize /\w:\zs/r0l1l0'}, {'lnum': 168, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'vim/vimrc.local', 'text': '  Tabularize /^[^=>]*\zs=>/l1'}, {'lnum': 177, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'vim/vimrc.local', 'text': '" Airline: Smarter tab line'}, {'lnum': 178, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'vim/vimrc.local', 'text': 'let g:airline#extensions#tabline#enabled = 1'}, {'lnum': 73, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'shell/git-completion.bash', 'text': '#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the'}, {'lnum': 278, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'shell/git-completion.bash', 'text': '# Execute ''git ls-files'', unless the --committable option is specified, in'}, {'lnum': 287, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'shell/git-completion.bash', 'text': '		if [ "$2" == "--committable" ]; then'}, {'lnum': 512, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'shell/git-completion.bash', 'text': '# The exception is --committable, which finds the files appropriate commit.'}, {'lnum': 1150, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'shell/git-completion.bash', 'text': '		__git_complete_index_file "--committable"'}, {'lnum': 2027, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'shell/git-completion.bash', 'text': '		core.deltaBaseCacheLimit'}, {'lnum': 2095, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'shell/git-completion.bash', 'text': '		gitcvs.dbTableNamePrefix'}, {'lnum': 2200, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'shell/git-completion.bash', 'text': '		repack.usedeltabaseoffset'}, {'lnum': 2733, 'col': 0, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'shell/git-completion.bash', 'text': '# when the user has tab-completed the executable name and consequently'}])
let SessionLoad = 1
if &cp | set nocp | endif
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/.dotfiles
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +1 ~/.dotfiles
badd +98 vim/vimrc.local
badd +86 home/vimrc
badd +0 shell/prompt-zsh.sh
argglobal
silent! argdel *
argadd ~/.dotfiles
edit home/vimrc
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 84 + 84) / 169)
exe 'vert 2resize ' . ((&columns * 84 + 84) / 169)
argglobal
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 18 - ((17 * winheight(0) + 17) / 34)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
18
normal! 0
lcd ~/.dotfiles
wincmd w
argglobal
edit ~/.dotfiles/vim/vimrc.local
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 59 - ((11 * winheight(0) + 17) / 34)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
59
normal! 0
lcd ~/.dotfiles
wincmd w
2wincmd w
exe 'vert 1resize ' . ((&columns * 84 + 84) / 169)
exe 'vert 2resize ' . ((&columns * 84 + 84) / 169)
tabnext 1
if exists('s:wipebuf')
"   silent exe 'bwipe ' . s:wipebuf
endif
" unlet! s:wipebuf
set winheight=999 winwidth=84 shortmess=filnxtToO
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save

" Support for special windows like quick-fix and plug-in windows.
" Everything down here is generated by vim-session (not supported
" by :mksession out of the box).

2wincmd w
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
