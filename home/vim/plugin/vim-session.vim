" The *g:session_directory* option
"
" This option controls the location of your session scripts. Its default value is
" '~/.vim/sessions' (on UNIX) or '~\vimfiles\sessions' (on Windows). If you don't
" mind the default you don't have to do anything; the directory will be created
" for you. Note that a leading '~' is expanded to your current home directory
" ('$HOME' on UNIX, '%USERPROFILE%' on Windows).
let g:session_directory = '.'

" The *g:session_autoload* option
"
" By default this option is set to "'prompt'". This means that when you start Vim
" without opening any files and the default session script exists, the session
" plug-in will ask whether you want to restore your default session. When you set
" this option to "'yes'" and you start Vim without opening any files the default
" session will be restored without a prompt. To completely disable automatic
" loading you can set this option to "'no'".
let g:session_autoprompt = 'yes'

" -------------------------------------------------------------------------------
" The *g:session_autosave* option
"
" By default this option is set to "'prompt'". When you've opened a session and
" you quit Vim, the session plug-in will ask whether you want to save the changes
" to your session. Set this option to "'yes'" to always automatically save open
" sessions when you quit Vim. To completely disable automatic saving you can set
" this option to "'no'".
let g:session_autosave = 'yes'
let g:session_autosave_periodic = 5 " save every 5 minutes

" The *g:session_persist_colors* option
"
" By default the plug-in will save the color scheme and the |'background'| option
" with the session to be reused the next time that session is loaded, this can be
" disabled by adding the following line to your |vimrc| script:

let g:session_persist_colors = 0
