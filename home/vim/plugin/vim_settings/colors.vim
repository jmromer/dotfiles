" Base vim settings
set t_Co=256  " 256 color terminal

" Allow italic text in vim
set t_ZH=[3m
set t_ZR=[23m

" Color scheme settings
if has("gui_running")
  set background=light
else
  set background=dark
  let g:solarized_termcolors=16
endif

colorscheme solarized
