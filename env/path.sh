if [[ -z "${DOTFILES_DIR}" ]]; then
  echo "WARNING: DOTFILES_DIR env var is not set in ${0}"
fi

if [[ -z "${XDG_DATA_HOME}" ]]; then
  echo "WARNING: XDG_DATA_HOME env var is not set in ${0}"
fi

if [[ -z "${HOMEBREW_PREFIX}" ]]; then
  echo "WARNING: HOMEBREW_PREFIX env var is not set in ${0}"
fi

PATH="${DOTFILES_DIR}/bin"

PATH+=":${XDG_DATA_HOME}/asdf-versions/shims"
PATH+=":${XDG_DATA_HOME}/asdf-manager/bin"
PATH+=":${XDG_DATA_HOME}/fzf/bin"
PATH+=":${XDG_DATA_HOME}/git/bin"
PATH+=":${XDG_DATA_HOME}/emacs/bin"
PATH+=":${XDG_DATA_HOME}/npm/bin"
PATH+=":${XDG_DATA_HOME}/gem/ruby/3.0.0/bin"

PATH+=":${HOMEBREW_PREFIX}/opt/coreutils/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/curl/bin"
PATH+=":${HOMEBREW_PREFIX}/opt/findutils/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/gettext/bin"
PATH+=":${HOMEBREW_PREFIX}/opt/gnu-bin/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/gnu-indent/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/gnu-sed/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/gnu-tar/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/gnu-which/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/gnutls/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/make/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/rg/bin"
PATH+=":${HOMEBREW_PREFIX}/share/git-core/contrib/diff-highlight"

PATH+=":${HOMEBREW_PREFIX}/bin"
PATH+=":${HOMEBREW_PREFIX}/sbin"
PATH+=":/usr/bin"
PATH+=":/bin"
PATH+=":/usr/sbin"
PATH+=":/sbin"

PATH+=":/Library/TeX/texbin"
export PATH