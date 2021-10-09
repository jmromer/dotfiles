if [[ -z "${DOTFILES_DIR}" ]]; then
  echo "WARNING: DOTFILES_DIR env var is not set in ${0}"
fi

if [[ -z "${XDG_DATA_HOME}" ]]; then
  echo "WARNING: XDG_DATA_HOME env var is not set in ${0}"
fi

if [[ -z "${HOMEBREW_PREFIX}" ]]; then
  echo "WARNING: HOMEBREW_PREFIX env var is not set in ${0}"
fi

PATH="${XDG_LOCALS_DIR}/bin"
PATH+=":${DOTFILES_DIR}/bin"
PATH+=":${XDG_SECURE_DIR}/bin"
PATH+=":./node_modules/.bin"

PATH+=":${XDG_DATA_HOME}/asdf-versions/shims"
PATH+=":${XDG_DATA_HOME}/asdf-manager/bin"
PATH+=":${XDG_DATA_HOME}/fzf/bin"
PATH+=":${XDG_DATA_HOME}/git/bin"
PATH+=":${XDG_DATA_HOME}/emacs/bin"
PATH+=":${XDG_DATA_HOME}/npm/bin"
PATH+=":${XDG_DATA_HOME}/gem/ruby/3.0.0/bin"

PATH+=":${HOMEBREW_PREFIX}/bin"
PATH+=":${HOMEBREW_PREFIX}/sbin"

PATH+=":${HOMEBREW_PREFIX}/opt/${OPENSSL:-openssl@1.1}/bin"
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
PATH+=":${HOMEBREW_PREFIX}/opt/grep/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/make/libexec/gnubin"
PATH+=":${HOMEBREW_PREFIX}/opt/rg/bin"

PATH+=":${HOMEBREW_PREFIX}/share/git-core/contrib/diff-highlight"

PATH+=":/usr/bin"
PATH+=":/bin"
PATH+=":/usr/sbin"
PATH+=":/sbin"

PATH+=":/Library/TeX/texbin"
export PATH

# Prefer BSD uname, becuase it reports m1 arch as 'arm'
# GNU uname reports it as 'arm64', which breaks some build scripts
if [[ -d "${HOMEBREW_PREFIX}/bin" ]]; then
  ln -sf /usr/bin/uname "${HOMEBREW_PREFIX}/bin/uname"
fi
