printf "${ZDOTDIR/$HOME/~}/.zshrc ... "

fpath=(
  ${BREW_PREFIX}/share/zsh/site-functions
  ${ASDF_DIR}/completions 
  $fpath
)
autoload -Uz compinit
compinit

export PS1="%1~ %# "
