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
# ALIASES: NODE, NPM, GREP
#-------------------------------------------------------------
alias npmls='npm ls -depth=0'
alias grep='GREP_COLOR="33;40" LANG=C grep --color=auto'

#-------------------------------------------------------------
# ALIASES: RUBY, RAILS
#-------------------------------------------------------------
# bundler
alias bi='bundle install' # -j3 should be set in ~/.bundle/config
alias bu='bundle update'
alias be='bundle exec'

# rails
alias rs='bin/rails server'
alias rc='bin/rails console'
alias rcs='bin/rails console --sandbox'

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

