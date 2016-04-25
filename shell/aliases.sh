# shell/aliases.sh
#-------------------------------------------------------------
# ALIASES: Editors, Tmux
#-------------------------------------------------------------
alias vi='nvim -u ~/.vim/minimal.vim'
alias vimplug='nvim +PlugUpdate +PlugClean! +qall'
alias brew='TERM=screen-256color brew'

#-------------------------------------------------------------
# ALIASES: FILE MANAGEMENT, SHELL NAVIGATION
#-------------------------------------------------------------
alias ..='\cd ..; l'         # go to parent dir and list contents
alias ...='\cd ../..; l'     # go to grandparent dir and list contents
alias mkdir='mkdir -p'      # create subdirectories as necessary
alias h='history'           # show history
alias l='gls --color'       # use GNU ls
alias la='l -A'             # list all files
alias ll='l -loh'           # show extended listing (but not all files)
alias llg='l -lh'           # show extended listing with group
alias lla='l -Aloh'         # all files, info, hide group, short sizes
alias llag='l -Al'          # all files, info, show group, short sizes
alias l.='l -d .*'          # show only files beginning with a dot
alias ll.='l -dloh .*'      # extended listing of files beginning with a dot
alias lt='tree'             # show directory as tree
alias d='dirs -v'           # show directory stack
alias c='clear'             # clear shell output
alias q='exit'              # quit the current process

#-------------------------------------------------------------
# ALIASES: SAFEGUARDS
#-------------------------------------------------------------
alias rm='rm -i'            # confirm deletion
alias mv='mv -i'            # confirm move if overwriing existing file
alias cp='cp -i'            # confirm copy if overwriting existing file
alias ln='ln -iv'           # display error if link exists; link verbosely

#-------------------------------------------------------------
# ALIASES: MISC
#-------------------------------------------------------------
alias diff='colordiff'        # compare files, colorize output
alias hide='setfile -a V'     # hide a file
alias unhide='setfile -a v'   # unhide a file
alias v='vagrant'
alias npmls='npm ls -depth=0'
alias grep='GREP_COLOR="33;40" LANG=C grep --color=auto'
alias ssh='TERM=xterm-256color ssh'

#-------------------------------------------------------------
# ALIASES: BUNDLER
#-------------------------------------------------------------
# bundler
alias bi='bundle install' # -j3 should be set in ~/.bundle/config
alias bu='bundle update'
alias be='bundle exec'

#-------------------------------------------------------------
# ALIASES: HEROKU
#-------------------------------------------------------------
# heroku
alias hr='heroku run'
alias hrk='heroku run rake'
alias hlg='heroku logs --tail'

function heroku-reset() {
  hreset "$1"
  heroku run rake db:migrate db:seed
}

function heroku-dropdb() {
  heroku pg:reset DATABASE --confirm "$1"
}
