source $XDG_CONFIG_HOME/vim/vimrc.minimal.vim

" Rebind 'q' in normal mode to quit fast (like ZQ / less)
nnoremap q :q!<CR>

" Rebind 'y' to copy to system clipboard
nnoremap y "+y
vnoremap y "+y
