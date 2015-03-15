# shell/aliases.sh
# NB: Disable alias by running with backslash, e.g.: \grep
#-------------------------------------------------------------
# ALIASES: Editors
#-------------------------------------------------------------
alias vi='vim -u ~/.dotfiles/vim/minimal.vim'
alias vimplug='vim +PlugUpdate +PlugClean! +qall'

function e() {
  if [ -z "$1" ]; then
    vim .
  else
    vim "$1"
  fi
}

#-------------------------------------------------------------
# ALIASES: Editors (note: postfix aliases unsupport in Bash)
#-------------------------------------------------------------
alias ln='ln -v'           # create links verbosely
alias tlf='tail -f'        # watch file

#-------------------------------------------------------------
# ALIASES: FILE MANAGEMENT, SHELL NAVIGATION
#-------------------------------------------------------------
alias ..='\cd ..;ls'        # go to parent dir and list contents
alias ...='\cd ../..;ls'    # go to grandparent dir and list contents
alias _='\cd -;ls'          # go to previous wd and list contents
alias mkdir='mkdir -p'      # create subdirectories as necessary
alias desk='\cd ~/Desktop'  # go to desktop
alias dev='\cd ~/Developer' # go to developer folder
alias h='history'           # show history
alias l='ls --color'        # list files (--color flag only works with GNU ls)
alias la='ls -A'            # list all files
alias ll='ls -loh'          # show extended listing (but not all files)
alias llg='ls -lh'          # show extended listing with group
alias lla='ls -Aloh'        # all files, info, hide group, short sizes
alias llag='ls -Al'         # all files, info, show group, short sizes
alias l.='ls -d .*'         # show only files beginning with a dot
alias ll.='ls -dloh .*'     # extended listing of files beginning with a dot
alias d='dirs -v'           # show directory stack
alias lt='tree'             # show directory as tree
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
# ALIASES: MAC STUFF, MISC
#-------------------------------------------------------------
alias diff='colordiff'        # compare files, colorize output
alias hide='setfile -a V'     # hide a file
alias unhide='setfile -a v'   # unhide a file
alias octave='octave -q'      # suppress greeting and version message

#-------------------------------------------------------------
# ALIASES: TODO.TXT
#-------------------------------------------------------------
export TODOTXT_DEFAULT_ACTION=ls
alias todo="todo.sh -d $HOME/.dotfiles/todo.cfg"
alias t='todo'
alias tadd='todo add'         # add a todo item
alias tapp='todo append'      # append text $2 to the task # $1
alias taddm='todo addm'       # add multiple todo items
alias tls='todo list'         # list all todos
alias tll='todo listall'      # list all todos, including completed
alias tpri='todo pri'         # set priority of $1 [num] to $2 [A-Z]
alias tdp='todo depri'        # removes priority from $1
alias tdone='todo do'         # mark item $1 as done
alias trm='todo rm'           # delete item $1

#-------------------------------------------------------------
# ALIASES: NODE, NPM, GREP
#-------------------------------------------------------------
alias npmls='npm ls -depth=0'
alias grep='GREP_COLOR="33;40" LANG=C grep --color=auto'

#-------------------------------------------------------------
# ALIASES: BUSTER
#-------------------------------------------------------------
alias bgd='buster generate && buster deploy'

#-------------------------------------------------------------
# ALIASES: RUBY, RAILS
#-------------------------------------------------------------
# bundler
alias bi='bundle install' # -j3 should be set in ~/.bundle/config
alias bu='bundle update'
alias rk='rake'
alias be='bundle exec'

# rails
alias rs='rails server'
alias rc='rails console'
alias rcs='rails console --sandbox'
alias rss='rake db:reset db:seed && rails server'
alias s='rspec --format progress'

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

